{-# LANGUAGE TemplateHaskell, ConstraintKinds #-}
{- |
Module      :  Lambda.Driver
Description :  Compiler driver.
Copyright   :  (c) Paweł Nowak
License     :  Apache v2.0

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
Portability :  portable

-}
 
module Lambda.Driver where

import System.IO
import Control.Lens
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.IO.Class
import Data.Text (Text)
import Text.Parsec (parse)
import Text.PrettyPrint (render)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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

-- | Driver targets.
data Target = DumpAST
            | DumpTypedAST
            | Evaluate
    deriving (Eq, Show, Read, Bounded, Enum)

-- | Driver configuration.
data Config = Config
    { _input :: Maybe FilePath
    , _output :: Maybe FilePath
    , _target :: Target
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
readInput :: MonadDriver m => ContT () m (Text, FilePath)
readInput = do
    input' <- view input
    case input' of
         Just fileName -> do
             text <- liftIO $ Text.readFile fileName
             return (text, fileName)
         Nothing -> do
             text <- liftIO $ Text.getContents
             return (text, "standard input")

-- | Prints something to the output.
printOutput :: (MonadDriver m, PrettyPrint t) => t -> m ()
printOutput t = do
    output' <- view output
    case output' of
         Just fileName -> liftIO . writeFile fileName . render . format $ t
         Nothing -> liftIO . hPutStr stderr . render . format $ t

-- | Parses the input.
parseInput :: MonadDriver m => (Text, FilePath) -> ContT () m [Stmt PExpr]
parseInput (text, filePath) = ContT $ \cont ->
    case parse program filePath text of
         Left err -> liftIO . print $ err
         Right stmts -> do
             target' <- view target
             if target' == DumpAST
                then printOutput $ stmts
                else cont stmts

-- | Transforms untyped AST into typed AST.
inferTypes :: MonadDriver m => [Stmt PExpr] -> ContT () m [Stmt TPExpr]
inferTypes stmts = ContT $ \cont ->
    case runInfer (inferStmts stmts) initBindings of
         Left err -> printError err
         Right (_, typedStmts) -> do
             target' <- view target
             if target' == DumpTypedAST
                then printOutput $ typedStmts
                else cont typedStmts
    where 
        initBindings = Map.fromList $ map extractBinding (primOps :: [PrimOp Eval])
        extractBinding (PrimOp n s _) = (n, s)

-- | Generates objects from a typed AST.
genObjs :: MonadDriver m => [Stmt TPExpr] -> ContT () m (LObject Eval)
genObjs stmts = ContT $ \cont -> do
    -- Initialize PrimOps.
    let primOpsMap = Map.fromList (map (\(PrimOp n _ o) -> (n, o)) (primOps :: [PrimOp Eval]))
        instPrimOps = Tr.sequence primOpsMap
    res <- liftIO $ runEval instPrimOps
    case res of
         Left err -> printErrorStr ("Internal error during PrimOps initialization: " ++ err)
         Right binds -> do
             obj <- liftIO $ runObjGen (objGenStmts stmts) binds
             cont obj

-- | Evaluate the object.
evalObj :: MonadDriver m => LObject Eval -> ContT () m ()
evalObj obj = do
    r <- liftIO $ runEval (eval obj)
    case r of
         Left err -> liftIO $ putStrLn ("Error: " ++ err)
         Right _ -> return ()

driver :: MonadDriver m => m ()
driver = runContT (readInput >>= parseInput >>= inferTypes >>= genObjs >>= evalObj) (return . return ())
