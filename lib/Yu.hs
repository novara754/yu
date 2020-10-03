{-|
Module      : Yu.Error
Description : Main module.

This module contains utility functions for parsing Yu code.
-}
module Yu
  ( parse
  , staticAnalyze
  , printAST
  , printParseErrors
  , printAnalysisErrors
  ) where

import           Control.Monad.Writer
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import           Hectoparsec
import           Errata
import           Text.Pretty.Simple

import           Yu.Syntax.Lexer
import           Yu.Syntax.Parser
import           Yu.Syntax.Error
import qualified Yu.StaticAnalysis as SA
import           Yu.StaticAnalysis.Types

-- | Helper function to parse raw source code into an AST.
parse :: FilePath
      -> T.Text
      -> Either (ParseError TokStream CustomParserError CustomLabel) (Module 'Parse)
parse fp src = do
  case evalParser pLexer fp src of
    Left _   -> error "testParse: lexer cannot error"
    Right ts -> evalParser pModule fp (TokStream ts)

-- | Run static analysis.
staticAnalyze :: Module 'Parse -> Either [SA.StaticAnalysisError] (Module 'TypeCheck)
staticAnalyze m =
  let (m', es) = runWriter $ SA.applyAll m
  in case es of
    [] -> Right m'
    _  -> Left es

-- | Pretty print an AST.
printAST :: (Module 'TypeCheck) -> IO ()
printAST = pPrint

-- | Pretty print errors that occured during parsing.
printParseErrors :: T.Text -> ParseError TokStream CustomParserError CustomLabel -> IO ()
printParseErrors src es = TL.putStrLn $ prettyErrors src [parseErrorErrata es]

-- | Pretty print errors that occured during static analysis.
printAnalysisErrors :: T.Text -> [StaticAnalysisError] -> IO ()
printAnalysisErrors src es = TL.putStrLn . prettyErrors src $ map analysisErrorErrata es
