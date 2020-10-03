{-|
Module      : Yu.Syntax.AST
Description : Abstract syntax tree.

Types for the abstract syntax tree.
-}

module Yu.Syntax.AST
  ( Phase(..)
  , Tok(..)
  , isIdent
  , isOpIdent
  , isInt
  , isBool
  , TokStream(..)
  , Identifier
  , Module(..)
  , Param(..)
  , Decl(..)
  , Stmt(..)
  , LiteralValue(..)
  , Expr(..)
  , NamePath
  , YuType(..)
  , getType
  ) where

import           Data.List (uncons)
import qualified Data.Text as T
import           Data.Kind
import           Hectoparsec

import           Yu.Syntax.Span

---------------------
-- Compiler Phases --
---------------------
data Phase
  = NoDec
  | Parse
  | NameRes
  | TypeCheck

-------------------
-- Type families --
-------------------

type family XParam        (d :: Phase)

type family XModuleDecl   (d :: Phase)
type family XFunctionDecl (d :: Phase)

type family XVarDecl      (d :: Phase)
type family XExprStmt     (d :: Phase)
type family XReturn       (d :: Phase)

type family XLiteral      (d :: Phase)
type family XVarRef       (d :: Phase)
type family XBinOp        (d :: Phase)
type family XFuncCall     (d :: Phase)
type family XGrouped      (d :: Phase)

type XForAll (c :: Type -> Constraint) (d :: Phase) =
  ( c (XModuleDecl d)
  , c (XFunctionDecl d)
  , c (XVarDecl d)
  , c (XExprStmt d)
  , c (XReturn d)
  , c (XLiteral d)
  , c (XVarRef d)
  , c (XBinOp d)
  , c (XFuncCall d)
  , c (XGrouped d)
  , c (XParam d)
  )

-----------------
-- Token types --
-----------------

-- | A token.
data Tok
  = Ident T.Text
  | OpIdent T.Text
  | Keyword T.Text
  | IntLiteral Integer
  | BoolLiteral Bool
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Colon
  | Semicolon
  | Comma
  | Eq
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

-- | Check whether a token is a boolean.
isBool :: Tok -> Bool
isBool (BoolLiteral _) = True
isBool _               = False

--------------------------
-- Abstract Syntax Tree --
--------------------------

-- | Token stream.
newtype TokStream = TokStream [Located Tok]

instance Stream TokStream where
  type Token TokStream = Located Tok
  type Chunk TokStream = [Located Tok]

  streamUncons (TokStream xs) = fmap TokStream <$> uncons xs
  updatePosToken _ lt _ = let Span fp _ (l, c) = lSpan lt in Pos fp l c

-- | An identifier for variables, functions, etc.
type Identifier = Located T.Text

-- | Represents function parameters.
data Param d = Param
  { paramAnn  :: XParam d
  , paramName :: Identifier
  , paramType :: Identifier
  }

deriving instance Show (XParam d) => Show (Param d)
deriving instance Eq (XParam d) => Eq (Param d)
deriving instance Ord (XParam d) => Ord (Param d)

-- | Represents a full Yu module.
data Module d = Module [Decl d]

deriving instance XForAll Show d => Show (Module d)
deriving instance XForAll Eq d => Eq (Module d)
deriving instance XForAll Ord d => Ord (Module d)

-- | Represents a top level declaration in a module.
data Decl d
  = ModuleDecl (XModuleDecl d) Identifier
  | FunctionDecl (XFunctionDecl d) Identifier [Param d] (Maybe Identifier) [Stmt d]

deriving instance XForAll Show d => Show (Decl d)
deriving instance XForAll Eq d => Eq (Decl d)
deriving instance XForAll Ord d => Ord (Decl d)

instance HasSpan (Decl 'Parse) where
  span (ModuleDecl   s _)       = s
  span (FunctionDecl s _ _ _ _) = s

-- | Represents statements inside of function bodies.
data Stmt d
  = VarDecl (XVarDecl d) Identifier (Maybe Identifier) (Expr d)
  | ExprStmt (XExprStmt d) (Expr d)
  | Return (XReturn d) (Expr d)

deriving instance XForAll Show d => Show (Stmt d)
deriving instance XForAll Eq d => Eq (Stmt d)
deriving instance XForAll Ord d => Ord (Stmt d)

instance HasSpan (Stmt 'Parse) where
  span (VarDecl  s _ _ _) = s
  span (ExprStmt s _)     = s
  span (Return   s _)     = s

-- | Type of literal.
data LiteralValue
  = LiteralInt Integer
  | LiteralBool Bool
  deriving (Show, Eq, Ord)

-- | Represents expressions yielding values.
data Expr d
  = Literal (XLiteral d) LiteralValue
  | VarRef (XVarRef d) Identifier
  | BinOp (XBinOp d) Identifier (Expr d) (Expr d)
  | FuncCall (XFuncCall d) (Expr d) [Expr d]
  | Grouped (XGrouped d) (Expr d)

deriving instance XForAll Show d => Show (Expr d)
deriving instance XForAll Eq d => Eq (Expr d)
deriving instance XForAll Ord d => Ord (Expr d)

instance HasSpan (Expr 'Parse) where
  span (Literal  s _)       = s
  span (VarRef   s _)       = s
  span (BinOp    s _ _ _)   = s
  span (FuncCall s _ _)     = s
  span (Grouped  s _)       = s

-----------------
-- Undecorated --
-----------------

type instance XParam        'NoDec = ()

type instance XModuleDecl   'NoDec = ()
type instance XFunctionDecl 'NoDec = ()

type instance XVarDecl      'NoDec = ()
type instance XExprStmt     'NoDec = ()
type instance XReturn       'NoDec = ()

type instance XLiteral      'NoDec = ()
type instance XVarRef       'NoDec = ()
type instance XBinOp        'NoDec = ()
type instance XFuncCall     'NoDec = ()
type instance XGrouped      'NoDec = ()

-------------------
-- Parsing Phase --
-------------------

type instance XParam        'Parse = Span

type instance XModuleDecl   'Parse = Span
type instance XFunctionDecl 'Parse = Span

type instance XVarDecl      'Parse = Span
type instance XExprStmt     'Parse = Span
type instance XReturn       'Parse = Span

type instance XLiteral      'Parse = Span
type instance XVarRef       'Parse = Span
type instance XBinOp        'Parse = Span
type instance XFuncCall     'Parse = Span
type instance XGrouped      'Parse = Span

---------------------------
-- Name Resolution Phase --
---------------------------

-- | Fully qualified path of a reference.
type NamePath = [T.Text]

type instance XParam        'NameRes = (Span, NamePath)

type instance XModuleDecl   'NameRes = (Span)
type instance XFunctionDecl 'NameRes = (Span, NamePath)

type instance XVarDecl      'NameRes = (Span, NamePath)
type instance XExprStmt     'NameRes = (Span)
type instance XReturn       'NameRes = (Span)

type instance XLiteral      'NameRes = (Span)
type instance XVarRef       'NameRes = (Span, Maybe NamePath)
type instance XBinOp        'NameRes = (Span)
type instance XFuncCall     'NameRes = (Span)
type instance XGrouped      'NameRes = (Span)

-------------------------
-- Type Checking Phase --
-------------------------

-- | Data types in the Yu language.
data YuType
  = YuVoid
  | YuInt
  | YuBool
  | YuFunc [YuType] YuType
  | YuUnknownType
  deriving (Show, Eq, Ord)

type instance XParam        'TypeCheck = (Span, NamePath)

type instance XModuleDecl   'TypeCheck = (Span)
type instance XFunctionDecl 'TypeCheck = (Span, NamePath)

type instance XVarDecl      'TypeCheck = (Span, NamePath)
type instance XExprStmt     'TypeCheck = (Span)
type instance XReturn       'TypeCheck = (Span)

type instance XLiteral      'TypeCheck = (Span, YuType)
type instance XVarRef       'TypeCheck = (Span, Maybe NamePath, YuType)
type instance XBinOp        'TypeCheck = (Span, YuType)
type instance XFuncCall     'TypeCheck = (Span, YuType)
type instance XGrouped      'TypeCheck = (Span, YuType)

-- | Get type of expression.
getType :: (Expr 'TypeCheck) -> YuType
getType (Literal (_, t) _)    = t
getType (VarRef (_, _, t) _)  = t
getType (BinOp (_, t) _ _ _)  = t
getType (FuncCall (_, t) _ _) = t
getType (Grouped (_, t) _ )   = t

instance HasSpan (Expr 'TypeCheck) where
  span (Literal (s, _) _)    = s
  span (VarRef (s, _, _) _)  = s
  span (BinOp (s, _) _ _ _)  = s
  span (FuncCall (s, _) _ _) = s
  span (Grouped (s, _) _ )   = s
