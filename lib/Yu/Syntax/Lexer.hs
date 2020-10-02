{-|
Module      : Yu.Syntax.Lexer
Description : Lexer for the Yu language.

This module contains types and parsers required to lex Yu source code.
-}

module Yu.Syntax.Lexer
 ( Span(..)
 , Tok(..)
 , Parser
 , pLexer
 , isIdent
 , isOpIdent
 , isInt
 ) where

import           Data.Void
import           Data.Char
import qualified Data.Text as T
import           Control.Monad
import           Control.Monad.Combinators
import           Hectoparsec hiding (Parser)
import qualified Hectoparsec as H

import           Yu.Syntax.AST
import           Yu.Syntax.Span

-------------
-- Parsers --
-------------

-- | Parser for lexing the raw input text.
type Parser = H.Parser T.Text Void Void

-- | The lexer.
pLexer :: Parser [Located Tok]
pLexer = do
  trivia
  xs <- many pToken
  eof <- pEof
  pure $ xs <> [eof]

-- | Parser for all possible tokens
pToken :: Parser (Located Tok)
pToken = choice
  [ try pKeyword
  , pSymbol
  , pIdent
  , pOpIdent
  , pInt
  , pError
  ]

-- | Parser for keywords.
pKeyword :: Parser (Located Tok)
pKeyword = located $ do
  kw <- choice $ map string ["module", "fun", "let", "return"]
  notFollowedBy $ satisfy isAlphaNum
  pure $ Keyword kw

-- | Parser for symbols.
pSymbol :: Parser (Located Tok)
pSymbol = choice
  [ symbol "("  LParen
  , symbol ")"  RParen
  , symbol "{"  LBrace
  , symbol "}"  RBrace
  , symbol ":"  Colon
  , symbol ";"  Semicolon
  , symbol ","  Comma
  , symbol "="  Eq
  ]

-- | Parser for identifiers.
pIdent :: Parser (Located Tok)
pIdent = located $ do
  x <- satisfy $ legalIdent False
  xs <- tokenWhile $ legalIdent True
  pure . Ident $ T.cons x xs

-- | Parser for operators.
pOpIdent :: Parser (Located Tok)
pOpIdent = located $ do
  x <- tokenWhile1 $ \c -> isPunctuation c || isSymbol c
  pure $ OpIdent x

-- | Parser for integer literals.
pInt :: Parser (Located Tok)
pInt = located $ do
  xs <- tokenWhile1 isDigit
  pure . IntLiteral . read . T.unpack $ xs

-- | Parser for unknown/illegal characters.
pError :: Parser (Located Tok)
pError = located $ do
  notFollowedBy endOfInput
  x <- anyToken
  pure $ Unknown x

pEof :: Parser (Located Tok)
pEof = do
  a <- getPos
  endOfInput
  b <- getPos
  pure $ Located (spanPos a b) Eof

-- | Whether a character is a legal in identifiers.
legalIdent :: Bool -> Char -> Bool
legalIdent trailing c = isAlpha c || (trailing && isDigit c)

-- | Helper parser for symbols that represent themselves.
symbol :: T.Text -> Tok -> Parser (Located Tok)
symbol s t = located $ do
  _ <- string s
  pure t

-- | Helper parser to add location data to a parser result.
--   Also consumes trailing trivia.
located :: Parser a -> Parser (Located a)
located p = do
  a <- getPos
  v <- p
  b <- getPos
  trivia
  pure $ Located (spanPos a b) v

-- | Parser to handle trivia, i.e. whitespace and comments which are discarded.
trivia :: Parser ()
trivia = void . many . choice $
  [ void $ tokenWhile1 isSpace
  , void $ string "//" *> tokenWhile (/= '\n')
  , void $ string "/*" *> manyTill anyToken (string "*/")
  ]
