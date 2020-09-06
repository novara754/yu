module Yu.Lexer
 ( Located(..)
 , Span(..)
 , Tok(..)
 , Parser
 , pLexer
 , isIdent
 , isOpIdent
 , isInt
 ) where

import           Data.Char
import           Data.Void
import qualified Data.Text as T
import           Control.Monad
import           Hectoparsec.Text
import           Hectoparsec.Text.Char

-----------------
-- Token types --
-----------------

-- | A located item.
data Located a = Located
  { lSpan  :: Span
  , lValue :: a
  } deriving (Show, Eq, Ord)

-- | Location and span of a located item.
data Span = Span
  { spanFile    :: FilePath   -- ^ Filepath.
  , spanOffsets :: (Int, Int) -- ^ Start and end of the span in the input stream.
  , spanStart   :: (Int, Int) -- ^ Starting line:column.
  , spanEnd     :: (Int, Int) -- ^ Ending line:column.
  } deriving (Show, Eq, Ord)

instance Semigroup Span where
    Span fp (os1, oe1) ss1 se1 <> Span _ (os2, oe2) ss2 se2
        = Span
            fp
            (min os1 os2, max oe1 oe2)
            (if min os1 os2 == os1 then ss1 else ss2)
            (if max oe1 oe2 == oe1 then se1 else se2)

-- | Construct a span from two positions.
spanPos :: Pos -> Pos -> Span
spanPos (Pos f ls cs os) (Pos _ le ce oe) = Span f (os, oe) (ls, cs) (le, ce)

-- | A token.
data Tok
  = Ident T.Text
  | OpIdent T.Text
  | Keyword T.Text
  | IntLiteral Integer
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Colon
  | Semicolon
  | Comma
  | Eq
  | Arrow
  | Eof
  | Unknown Char -- Lexer does not report errors, simply passes them along to parser.
  deriving (Show, Eq, Ord)

-- | Check whether a token is an identifier.
isIdent :: Tok -> Bool
isIdent (Ident _) = True
isIdent _         = False
-- | Check whether a token is an operator.
isOpIdent :: Tok -> Bool
isOpIdent (OpIdent _) = True
isOpIdent _           = False

-- | Check whether a token is an integer.
isInt :: Tok -> Bool
isInt (IntLiteral _) = True
isInt _              = False

-------------
-- Parsers --
-------------

-- | Parser for lexing the raw input text.
type Parser = Partext T.Text Void T.Text

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
  xs <- tokensWhile $ legalIdent True
  pure . Ident $ T.cons x xs

-- | Parser for operators.
pOpIdent :: Parser (Located Tok)
pOpIdent = located $ do
  x <- tokensWhile1 $ \c -> isPunctuation c || isSymbol c
  pure $ OpIdent x

-- | Parser for integer literals.
pInt :: Parser (Located Tok)
pInt = located $ do
  xs <- tokensWhile1 isDigit
  pure . IntLiteral . read . T.unpack $ xs

-- | Parser for unknown/illegal characters.
pError :: Parser (Located Tok)
pError = located $ do
  notFollowedBy endOfInput
  x <- anyChar
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
  [ void spaces1
  , void $ string "//" *> tokensWhile (/= '\n')
  , void $ string "/*" *> manyTill anyToken (string "*/")
  ]

-- | The custom state is `Pos` because `Partext` is used.
getPos :: Parser Pos
getPos = getCustom
