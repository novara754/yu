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
import           Errata
import           Text.Pretty.Simple

import           Yu.Syntax.Lexer
import           Yu.Syntax.Parser
import           Yu.Syntax.Span
import           Yu.Syntax.Error

-- | Helper function to parse raw source code into an AST.
parse :: FilePath
      -> T.Text
      -> Either (ParseError [Located Tok] CustomParserError CustomLabel) (Module 'Parse)
parse fp src = do
  case evalLexer pLexer fp src of
    Left _   -> error "testParse: lexer cannot error"
    Right ts -> evalParser pModule ts

-- | Pretty print an AST.
printAST :: (Module 'Parse) -> IO ()
printAST = pPrint

-- | Pretty print errors that occured during parsing.
printErrors :: T.Text -> ParseError [Located Tok] CustomParserError CustomLabel -> IO ()
printErrors src es = TL.putStrLn $ prettyErrors src [parseErrorErrata es]
