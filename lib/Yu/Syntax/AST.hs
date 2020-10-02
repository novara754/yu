{-|
Module      : Yu.Syntax.AST
Description : Abstract syntax tree.

Types for the abstract syntax tree.
-}

module Yu.Syntax.AST
  ( Phase(..)
  , Identifier
  , Module(..)
  , Param(..)
  , Decl(..)
  , Stmt(..)
  , Expr(..)
  , NamePath
  ) where

import qualified Data.Text as T
import           Data.Kind

import           Yu.Syntax.Span

---------------------
-- Compiler Phases --
---------------------
data Phase
  = NoDec
  | Parse
  | NameRes

-------------------
-- Type families --
-------------------

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
  )

--------------------------
-- Abstract Syntax Tree --
--------------------------

-- | An identifier for variables, functions, etc.
type Identifier = Located T.Text

-- | Represents function parameters.
data Param = Param
  { paramName :: Identifier
  , paramType :: Identifier
  } deriving (Show, Eq, Ord)

-- | Represents a full Yu module.
data Module d = Module [Decl d]

deriving instance XForAll Show d => Show (Module d)
deriving instance XForAll Eq d => Eq (Module d)
deriving instance XForAll Ord d => Ord (Module d)

-- | Represents a top level declaration in a module.
data Decl d
  = ModuleDecl (XModuleDecl d) Identifier
  | FunctionDecl (XFunctionDecl d) Identifier [Param] (Maybe Identifier) [Stmt d]

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

-- | Represents expressions yielding values.
data Expr d
  = Literal (XLiteral d) Integer
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

type instance XModuleDecl   'NameRes = (Span)
type instance XFunctionDecl 'NameRes = (Span)

type instance XVarDecl      'NameRes = (Span)
type instance XExprStmt     'NameRes = (Span)
type instance XReturn       'NameRes = (Span)

type instance XLiteral      'NameRes = (Span)
type instance XVarRef       'NameRes = (Span, Maybe NamePath)
type instance XBinOp        'NameRes = (Span)
type instance XFuncCall     'NameRes = (Span)
type instance XGrouped      'NameRes = (Span)
