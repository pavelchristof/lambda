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
         Right exprs -> sequence_ $ do
             expr <- exprs
             return $ do
                 case runInfer $ infer expr of
                      Left err -> putStrLn . render . format $ err
                      Right typedExpr -> putStrLn . render . format $ typedExpr
                 putStrLn ""

main :: IO ()
main = do
    args <- getArgs
    case args of
         filePath:_ -> compile filePath
         _          -> putStrLn "Usage: lambda <input file>"
