{-|
Module      : Yu.Error
Description : Main module.

This module contains utility functions for parsing Yu code.
-}
module Yu
  ( parse
  , printAST
  , printErrors
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import           Hectoparsec
import           Hectoparsec.Text
import           Errata
import           Text.Pretty.Simple

import           Yu.Syntax.Lexer
import           Yu.Syntax.Parser
import           Yu.Error

-- | Helper function to parse raw source code into an AST.
parse :: FilePath
      -> T.Text
      -> Either (ParseErrors [Located Tok] () CustomError CustomLabel) Module
parse fp src = do
  case evalPartext pLexer fp src of
    Left _   -> error "testParse: lexer cannot error"
    Right ts -> evalParsec pModule () ts

-- | Pretty print an AST.
printAST :: Module -> IO ()
printAST = pPrint

-- | Pretty print errors that occured during parsing.
printErrors :: T.Text -> ParseErrors [Located Tok] () CustomError CustomLabel -> IO ()
printErrors src es = TL.putStrLn $ prettyErrorsNE src (parseErrorErrata <$> es)
