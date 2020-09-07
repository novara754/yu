module Yu.Syntax.Parser
 ( Param(..)
 , Module(..)
 , Decl(..)
 , Stmt(..)
 , Expr(..)
 , Parser
 , pModule
 ) where

import qualified Data.Text as T
import           Hectoparsec hiding (token, tokens)

import           Yu.Syntax.Lexer hiding (Parser)
import           Yu.Error

--------------------------
-- Abstract Syntax Tree --
--------------------------

type Identifier = Located T.Text

data Param = Param
  { paramName :: Identifier
  , paramType :: Identifier
  } deriving (Show, Eq, Ord)

data Module = Module
  { moduleDecls :: [Located Decl]
  } deriving (Show, Eq, Ord)

data Decl
  = ModuleDecl Identifier
  | FunctionDecl Identifier [Param] (Maybe Identifier) [Located Stmt]
  deriving (Show, Eq, Ord)

data Stmt
  = VarDecl Identifier (Maybe Identifier) (Located Expr)
  | ExprStmt (Located Expr)
  | Return (Located Expr)
  deriving (Show, Eq, Ord)

data Expr
  = ExprIntLiteral Integer
  | VarRef Identifier
  | BinOp Identifier (Located Expr) (Located Expr)
  | FuncCall (Located Expr) [Located Expr]
  deriving (Show, Eq, Ord)

-------------
-- Parsers --
-------------

-- | Token stream.
type TokStream = [Located Tok]

-- | Parser to turn tokens into an AST.
type Parser = Parsec TokStream () CustomError CustomLabel

-- | Parser for whole modules.
pModule :: Parser Module
pModule = do
  decls <- many pDecl <* token Eof
  pure $ Module decls

-- | Parser for top level declarations.
pDecl :: Parser (Located Decl)
pDecl = label LabelDeclaration $ choice
  [ pModuleDecl
  , pFuncDecl
  ]

-- | Parser for module declarations.
pModuleDecl :: Parser (Located Decl)
pModuleDecl = do
  kw <- token $ Keyword "module"
  name <- pIdent
  semi <- token $ Semicolon
  pure $ Located (lSpan kw <> lSpan semi) (ModuleDecl name)

-- | Parser for function declarations.
pFuncDecl :: Parser (Located Decl)
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
  pure $ Located (lSpan def <> lSpan close) (FunctionDecl name params retty body)
  where
    param = Param <$> pIdent <*> (token Colon >> pIdent)

-- | Parser for statements.
pStmt :: Parser (Located Stmt)
pStmt = label LabelStatement $ choice
  [ pVarDecl
  , pExprStmt
  , pReturn
  ]

-- | Parser for variable declarations.
pVarDecl :: Parser (Located Stmt)
pVarDecl = do
  let' <- token $ Keyword "let"
  name <- pIdent
  ty <- try . optional $ token Colon >> pIdent
  _ <- token Eq
  val <- pExpr
  semi <- token Semicolon
  pure $ Located (lSpan let' <> lSpan semi) (VarDecl name ty val)

-- | Parser for expression statements
pExprStmt :: Parser (Located Stmt)
pExprStmt = do
  x <- pExpr
  semi <- token Semicolon
  pure $ Located (lSpan x <> lSpan semi) (ExprStmt x)

-- | Parser for variable return statements.
pReturn :: Parser (Located Stmt)
pReturn = do
  ret <- token $ Keyword "return"
  val <- pExpr
  semi <- token Semicolon
  pure $ Located (lSpan ret <> lSpan semi) (Return val)

-- | Parser for full expressions.
pExpr :: Parser (Located Expr)
pExpr = do
  f <- pBinOp
  calls <- many $ do
    start <- token LParen
    args <- sepBy pExpr (token Comma)
    end <- token RParen
    pure (args, lSpan start <> lSpan end)
  pure $ case calls of
    [] -> f
    ((c0, s0):cs) ->
      foldl
        (\acc (c, s) -> Located (lSpan acc <> s) (FuncCall acc c))
        (Located (lSpan f <> s0) (FuncCall f c0))
        cs

-- | Parser for binary operations.
pBinOp :: Parser (Located Expr)
pBinOp = do
  x <- pExprBase
  rest <- many $ (,) <$> pOpIdent <*> pExpr
  pure $ case rest of
    []      -> x
    ((op0, y0):ops) ->
      foldl
        (\acc (op, y) -> Located (lSpan acc <> lSpan y) (BinOp op acc y))
        (Located (lSpan x <> lSpan y0) (BinOp op0 x y0))
        ops

-- | Parser for base expressions (i.e. no binary operations and no function calls).
pExprBase :: Parser (Located Expr)
pExprBase = choice
  [ pLiteral
  , pVarRef
  , between (token LParen) (token RParen) pExpr
  ]

-- | Parser for literals.
pLiteral :: Parser (Located Expr)
pLiteral = label LabelInt $ do
  Located s (IntLiteral x) <- satisfy (isInt . lValue)
  pure $ Located s (ExprIntLiteral x)

-- | Parser for variable reference expressions.
pVarRef :: Parser (Located Expr)
pVarRef = do
  var <- pIdent
  pure $ Located (lSpan var) (VarRef var)

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
token t = matchToken $ \case
  Nothing -> error "token: reached end of stream"
  Just x  -> if lValue x == t
    then Right x
    else Left $ ErrorItemHints (Just $ HintToken x) [HintLabel $ LabelTok t]
