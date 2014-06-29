import System.Environment
import Text.Parsec
import Text.PrettyPrint (render)
import Data.Text (Text)
import qualified Data.Text.IO as Text

import Lambda.Parser
import Lambda.Inference
import Lambda.PrettyPrint

compile :: FilePath -> IO ()
compile filePath = do
    text <- Text.readFile filePath
    case parse program filePath text of
         Left err -> print err
         Right exprs -> do
             case runInfer $ mapM infer exprs of
                  Left err -> putStrLn . render . format $ err
                  Right typedExprs -> mapM_ (putStrLn . render . format) typedExprs

main :: IO ()
main = do
    args <- getArgs
    case args of
         filePath:_ -> compile filePath
         _          -> putStrLn "Usage: lambda <input file>"
