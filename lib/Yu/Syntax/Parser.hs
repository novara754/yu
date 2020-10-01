{-|
Module      : Yu.Syntax.Parser
Description : Parser for the Yu language.

This module contains types and parsers for the Yu language.
These parsers work on a token stream generated by "Yu.Syntax.Lexer".
-}
module Yu.Syntax.Parser
 ( module Yu.Syntax.AST
 , Parser
 , pModule
 ) where

import           Prelude hiding (span)
import           Control.Monad.Combinators
import           Hectoparsec.Parser hiding (Parser)
import qualified Hectoparsec.Parser as H

import           Yu.Syntax.Lexer hiding (Parser)
import           Yu.Syntax.Span
import           Yu.Syntax.AST
import           Yu.Syntax.Error

-- | Token stream.
type TokStream = [Located Tok]

-- | Parser to turn tokens into an AST.
type Parser = H.Parser TokStream CustomParserError CustomLabel

-- | Parser for whole modules.
pModule :: Parser (Module 'Parse)
pModule = do
  decls <- many pDecl <* token Eof
  pure $ Module decls

-- | Parser for top level declarations.
pDecl :: Parser (Decl 'Parse)
pDecl = label LabelDeclaration $ choice
  [ pModuleDecl
  , pFuncDecl
  ]

-- | Parser for module declarations.
pModuleDecl :: Parser (Decl 'Parse)
pModuleDecl = do
  kw <- token $ Keyword "module"
  name <- pIdent
  semi <- token $ Semicolon
  pure $ ModuleDecl (span kw <> span semi) name

-- | Parser for function declarations.
pFuncDecl :: Parser (Decl 'Parse)
pFuncDecl = do
  def <- token $ Keyword "fun"
  name <- pIdent
  _ <- token $ LParen
  params <- sepBy param (token Comma)
  _ <- token $ RParen
  retty <- try . optional $ token Colon >> pIdent
  _ <- token $ LBrace
  body <- many pStmt
  close <- token RBrace
  pure $ FunctionDecl (span def <> span close) name params retty body
  where
    param = Param <$> pIdent <*> (token Colon >> pIdent)

-- | Parser for statements.
pStmt :: Parser (Stmt 'Parse)
pStmt = label LabelStatement $ choice
  [ pVarDecl
  , pExprStmt
  , pReturn
  ]

-- | Parser for variable declarations.
pVarDecl :: Parser (Stmt 'Parse)
pVarDecl = do
  let' <- token $ Keyword "let"
  name <- pIdent
  ty <- try . optional $ token Colon >> pIdent
  _ <- token Eq
  val <- pExpr
  semi <- token Semicolon
  pure $ VarDecl (span let' <> span semi) name ty val

-- | Parser for expression statements
pExprStmt :: Parser (Stmt 'Parse)
pExprStmt = do
  x <- pExpr
  semi <- token Semicolon
  pure $ ExprStmt (span x <> span semi) x

-- | Parser for variable return statements.
pReturn :: Parser (Stmt 'Parse)
pReturn = do
  ret <- token $ Keyword "return"
  val <- pExpr
  semi <- token Semicolon
  pure $ Return (span ret <> span semi) val

-- | Parser for full expressions.
pExpr :: Parser (Expr 'Parse)
pExpr = do
  f <- pBinOp
  calls <- many $ do
    start <- token LParen
    args <- sepBy pExpr (token Comma)
    end <- token RParen
    pure (args, span start <> span end)
  pure $ case calls of
    [] -> f
    ((c0, s0):cs) ->
      foldl
        (\acc (c, s) -> FuncCall (span acc <> s) acc c)
        (FuncCall (span f <> s0) f c0)
        cs

-- | Parser for binary operations.
pBinOp :: Parser (Expr 'Parse)
pBinOp = do
  x <- pExprBase
  rest <- many $ (,) <$> pOpIdent <*> pExpr
  pure $ case rest of
    []      -> x
    ((op0, y0):ops) ->
      foldl
        (\acc (op, y) -> BinOp (span acc <> span y) op acc y)
        (BinOp (span x <> span y0) op0 x y0)
        ops

-- | Parser for base expressions (i.e. no binary operations and no function calls).
pExprBase :: Parser (Expr 'Parse)
pExprBase = choice
  [ pLiteral
  , pVarRef
  , pGrouped
  ]

-- | Parser for literals.
pLiteral :: Parser (Expr 'Parse)
pLiteral = label LabelInt $ do
  Located s (IntLiteral x) <- satisfy (isInt . lValue)
  pure $ Literal s x

-- | Parser for variable reference expressions.
pVarRef :: Parser (Expr 'Parse)
pVarRef = do
  var <- pIdent
  pure $ VarRef (span var) var

-- | Parser for grouped (parenthesised) expressions.
pGrouped :: Parser (Expr 'Parse)
pGrouped = do
  o <- token LParen
  v <- pExpr
  c <- token RParen
  pure $ Grouped (span o <> span c) v

-- | Helper parser for parsing identifiers and getting their text.
pIdent :: Parser Identifier
pIdent = label LabelIdent $ do
  Located s (Ident x) <- satisfy (isIdent . lValue)
  pure $ Located s x

-- | Helper parser for parsing identifiers and getting their text.
pOpIdent :: Parser Identifier
pOpIdent = label LabelIdent $ do
  Located s (OpIdent x) <- satisfy (isOpIdent . lValue)
  pure $ Located s x

-- | Helper parser to match tokens.
token :: Tok -> Parser (Located Tok)
token t = matchToken $ \x ->
  if lValue x == t
    then Right x
    else Left $ ErrorItemHints x [LabelTok t]
