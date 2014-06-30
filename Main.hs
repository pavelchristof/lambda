import System.Environment
import Text.Parsec
import Text.PrettyPrint (render)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lambda.Name
import Lambda.Type
import Lambda.Parser
import Lambda.Inference
import Lambda.PrettyPrint

initBindings = Map.fromList
    -- Integers
    [ ("+", TypeScheme Set.empty (TFun tInt (TFun tInt tInt)))
    , ("-", TypeScheme Set.empty (TFun tInt (TFun tInt tInt)))
    , ("/", TypeScheme Set.empty (TFun tInt (TFun tInt tInt)))
    , ("*", TypeScheme Set.empty (TFun tInt (TFun tInt tInt)))
    , ("==", TypeScheme Set.empty (TFun tInt (TFun tInt tBool)))
    , ("<", TypeScheme Set.empty (TFun tInt (TFun tInt tBool)))
    , ("<=", TypeScheme Set.empty (TFun tInt (TFun tInt tBool)))
    , (">", TypeScheme Set.empty (TFun tInt (TFun tInt tBool)))
    , (">=", TypeScheme Set.empty (TFun tInt (TFun tInt tBool)))

    -- Control flow.
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

compile :: FilePath -> IO ()
compile filePath = do
    text <- Text.readFile filePath
    case parse program filePath text of
         Left err -> print err
         Right stmts -> do
--              mapM_ (putStrLn . render . format) stmts
             case runInfer (inferStmts stmts) initBindings of
                  Left err -> putStrLn . render . format $ err
                  Right (_, typedStmts) -> putStrLn . render . format $ typedStmts

main :: IO ()
main = do
    args <- getArgs
    case args of
         filePath:_ -> compile filePath
         _          -> putStrLn "Usage: lambda <input file>"
