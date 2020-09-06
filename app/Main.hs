module Main (main) where

import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import           System.Environment
import           Hectoparsec
import           Hectoparsec.Text
import           Errata

import           Yu.Lexer
import           Yu.Parser
import           Yu.Error

-- | Helper function to read a file
parseAndPrint :: FilePath -> IO ()
parseAndPrint fp = do
  src <- T.readFile fp
  case evalPartext pLexer fp src of
    Left _   -> error "testParse: lexer cannot error"
    Right ts -> case evalParsec pModule () ts of
      Right x -> print x
      Left es -> TL.putStrLn $ prettyErrorsNE src (parseErrorErrata <$> es)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fp] -> parseAndPrint fp
    _    -> putStrLn "usage: yu <file>"
