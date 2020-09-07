module Main (main) where

import qualified Data.Text.IO as T
import           System.Environment
import           Yu

readParsePrint :: FilePath -> IO ()
readParsePrint fp = do
  src <- T.readFile fp
  case parse fp src of
    Left es   -> printErrors src es
    Right ast -> printAST ast


main :: IO ()
main = do
  args <- getArgs
  case args of
    [fp] -> readParsePrint fp
    _    -> putStrLn "usage: yu <file>"
