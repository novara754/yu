module Main (main) where

import qualified Data.Text.IO as T
import           Data.Foldable
import           System.Environment
import           Hectoparsec.Text

import Yu.Lexer

-- | Lex source code from a file and print the token stream.
lexAndPrint :: FilePath -> IO ()
lexAndPrint fp = do
  src <- T.readFile fp
  case evalPartext pLexer fp src of
    Left e   -> print e
    Right ts -> traverse_ (print . lValue) ts

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fp] -> lexAndPrint fp
    _    -> putStrLn "usage: yu <file>"
