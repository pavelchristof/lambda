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
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.IO.Class
import Data.Text (Text)
import Text.Parsec (parse)
import Text.PrettyPrint (render)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lambda.Name
import Lambda.Type
import Lambda.Syntax
import Lambda.Parser
import Lambda.Inference
import Lambda.PrettyPrint

-- | Initial bindings. TODO: move that to Prim.
initBindings = Map.fromList
    -- Integers
    [ ("+", TypeScheme Set.empty (TFun tInt (TFun tInt tInt)))
    , ("-", TypeScheme Set.empty (TFun tInt (TFun tInt tInt)))
    , ("/", TypeScheme Set.empty (TFun tInt (TFun tInt tInt)))
    , ("*", TypeScheme Set.empty (TFun tInt (TFun tInt tInt)))
    , ("<", TypeScheme Set.empty (TFun tInt (TFun tInt tBool)))
    , ("<=", TypeScheme Set.empty (TFun tInt (TFun tInt tBool)))
    , (">", TypeScheme Set.empty (TFun tInt (TFun tInt tBool)))
    , (">=", TypeScheme Set.empty (TFun tInt (TFun tInt tBool)))

    -- Control flow.
    , ("==", TypeScheme (Set.singleton "a") (TFun (TVar "a") (TFun (TVar "a") tBool)))
    , ("if", TypeScheme (Set.singleton "a") (TFun tBool (TFun (TVar "a") (TFun (TVar "a") (TVar "a")))))

    -- Lists.
    , (":", TypeScheme (Set.singleton "a") (TFun (TVar "a") (TFun (TList (TVar "a")) (TList (TVar "a")))))
    , ("head", TypeScheme (Set.singleton "a") (TFun (TList (TVar "a")) (TVar "a")))
    , ("tail", TypeScheme (Set.singleton "a") (TFun (TList (TVar "a")) (TList (TVar "a"))))
    , ("null", TypeScheme (Set.singleton "a") (TFun (TList (TVar "a")) tBool))

    -- IO.
    , ("printInt", TypeScheme Set.empty (TFun tInt tUnit))
    , ("printStr", TypeScheme Set.empty (TFun (TList tChar) tUnit))
    ]

-- | Driver targets.
data Target = DumpAST
            | DumpTypedAST
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

driver :: MonadDriver m => m ()
driver = runContT (readInput >>= parseInput >>= inferTypes) (return . return ())
