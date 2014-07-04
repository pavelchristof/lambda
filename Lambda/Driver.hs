{-# LANGUAGE TemplateHaskell, ConstraintKinds #-}
{- |
Module      :  Lambda.Driver
Description :  Compiler driver.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental

-}
 
module Lambda.Driver where

import System.IO
import Control.Lens hiding (Action)
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import Text.PrettyPrint (render)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Traversable as Tr

import Lambda.Name
import Lambda.Type
import Lambda.Syntax
import Lambda.Parser
import Lambda.PrimOp
import Lambda.Inference
import Lambda.Object
import Lambda.ObjGen
import Lambda.Eval
import Lambda.PrettyPrint
import Lambda.SourceLoc

-- | Driver targets.
data Action = DumpAST
            | DumpTypedAST
            | DumpObjects
            | Evaluate
    deriving (Eq, Show, Read, Bounded, Enum)

-- | Driver configuration.
data Config = Config
    { _input :: Maybe FilePath
    , _output :: Maybe FilePath
    , _target :: Action
    }
makeLenses ''Config

-- | Driver monad.
type MonadDriver m = (MonadReader Config m, MonadIO m)
type Driver = ReaderT Config IO

runDriver :: Driver a -> Config -> IO a
runDriver = runReaderT

-- | Prints an error to stderr.
printError :: (MonadIO m, PrettyPrint t) => t -> m ()
printError = liftIO . hPutStr stderr . render . format

-- | Prints an error string to stderr.
printErrorStr :: MonadIO m => String -> m ()
printErrorStr = liftIO . hPutStr stderr

-- | Reads the input, either from file or from stdin.
readInput :: MonadDriver m => ContT () m (ByteString, FilePath)
readInput = do
    input' <- view input
    case input' of
         Just fileName -> do
             text <- liftIO $ ByteString.readFile fileName
             return (text, fileName)
         Nothing -> do
             text <- liftIO $ ByteString.getContents
             return (text, "standard input")

-- | Prints something to the output.
printOutput :: MonadDriver m => String -> m ()
printOutput t = do
    output' <- view output
    case output' of
         Just fileName -> liftIO . writeFile fileName $ t
         Nothing -> liftIO . hPutStr stderr $ t

-- | Parses the input.
parseInput :: MonadDriver m => (ByteString, FilePath) -> ContT () m [Located (Decl LExpr)]
parseInput (text, filePath) = ContT $ \cont ->
    case parse text of
         Left err -> printErrorStr err
         Right decls -> do
             target' <- view target
             if target' == DumpAST
                then printOutput . render . format $ (map unLoc decls)
                else cont decls

-- | Transforms untyped AST into typed AST.
inferTypes :: MonadDriver m => [Located (Decl LExpr)] -> ContT () m [Located (Decl TLExpr)]
inferTypes decls = ContT $ \cont ->
    case runInfer (inferStmts decls) of
         Left err -> printError err
         Right (_, typedStmts) -> do
             target' <- view target
             if target' == DumpTypedAST
                then printOutput . render . format $ (map unLoc typedStmts)
                else cont typedStmts

-- | Generates objects from a typed AST.
genObjs :: MonadDriver m => [Located (Decl TLExpr)] -> ContT () m (Map Name (LObject Eval))
genObjs decls = ContT $ \cont -> do
    -- Initialize PrimOps.
    let primOpsMap = Map.fromList (map (\(PrimOp n _ o) -> (n, o)) (primOps :: [PrimOp Eval]))
                    `Map.union`
                     Map.fromList (map (\(PrimCons n _ o) -> (n, o)) (primCons :: [PrimCons Eval]))
        instPrimOps = Tr.sequence primOpsMap
    res <- liftIO $ runEval instPrimOps
    case res of
         Left err -> printErrorStr ("Internal error during PrimOps initialization: " ++ err)
         Right binds -> do
             -- Generate the bindings.
             objBinds <- liftIO $ runObjGen (objGenDecls (map unLoc decls)) binds
             target' <- view target
             if target' == DumpObjects
                then do
                    docs <- liftIO $ Tr.mapM formatIO objBinds
                    printOutput (render $ format docs)
                else cont objBinds

-- | Evaluate the main function.
evalObj :: MonadDriver m => Map Name (LObject Eval) -> ContT () m ()
evalObj objBinds = do
    case Map.lookup "main" objBinds of
         Just obj -> do
            r <- liftIO $ runEval (eval obj)
            case r of
                Left err -> printOutput ("Error: " ++ err)
                Right _ -> return ()
         Nothing -> printOutput ("No main.")

driver :: MonadDriver m => m ()
driver = runContT (readInput >>= parseInput >>= inferTypes >>= genObjs >>= evalObj) (return . return ())
