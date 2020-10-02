{-|
Module      : Yu.StaticAnalysis.NameResolution.Internal
Description : Name resolution of references.

Perform name resolution of references to attach
the location of definitions for variable, function, etc. references.

This module is internal and may break between non-major versions.
-}

module Yu.StaticAnalysis.NameResolution.Internal
  ( resolveNamesModule
  , resolveNamesDecl
  , resolveNamesStmt
  , resolveNamesExpr
  ) where

import           Data.Foldable
import qualified Data.Text as T
import           Data.Maybe
import           Control.Monad.Writer
import           Control.Monad.State

import           Yu.Syntax.AST
import           Yu.Syntax.Span
import           Yu.StaticAnalysis.Types

-- | Monad for name resolution.
type NameResolutionState a = WriterT [NameResolutionError] (State [DefinitionScope]) a

-- | Definition, possibly function scoped.
data Definition = Definition
  { local   :: Bool
  , defName :: Identifier
  } deriving (Show, Eq, Ord)

-- | Scope of definitions.
data DefinitionScope = DefinitionScope
  { scopeName :: T.Text
  , scopeDefs :: [Definition]
  } deriving (Show, Eq, Ord)

-- | Push a new named scope on the scope stack.
pushScope :: T.Text -> NameResolutionState ()
pushScope n = let d = DefinitionScope n []
              in modify (\ss -> d : ss)

-- | Remove the current scope from the scope stack.
popScope :: NameResolutionState ()
popScope = modify f
  where
    f []     = []
    f (_:ss) = ss

-- | Add a new definition to the current scope.
addDef :: Bool
       -> Identifier
       -> NameResolutionState ()
addDef l n = do
  scopes <- get
  let d = Definition l n
  let exists  =
        case scopes of
          []      -> Nothing
          scope:_ -> find (\d' -> lValue (defName d) == lValue (defName d')) (scopeDefs scope)
  case exists of
    Nothing -> modify (\((DefinitionScope n' ds):ss) -> (DefinitionScope n' (d:ds)):ss)
    Just e  -> tell [DuplicateDefinition n $ defName e]


hasDef :: T.Text -> DefinitionScope -> Bool
hasDef n (DefinitionScope _ d) = n `elem` map (lValue . defName) d

-- | Resolve names in a module.
resolveNamesModule :: Module 'Parse
                   -> NameResolutionState (Module 'NameRes)
resolveNamesModule (Module d) = do
  pushScope "<module>"
  traverse (addDef False) $ mapMaybe getName d
  d' <- traverse resolveNamesDecl d
  pure $ Module d'
  where
    getName (FunctionDecl _ n _ _ _) = Just n
    getName _                        = Nothing

-- | Resolve names in declarations.
resolveNamesDecl :: Decl 'Parse
                 -> NameResolutionState (Decl 'NameRes)
resolveNamesDecl (ModuleDecl s n)         = pure $ ModuleDecl s n
resolveNamesDecl (FunctionDecl s n p r b) = do
  pushScope $ lValue n
  addDef False n
  traverse_ (\p' -> addDef True $ paramName p') p
  b' <- traverse resolveNamesStmt b
  popScope
  pure $ FunctionDecl s n p r b'

-- | Resolve names in statements.
resolveNamesStmt :: Stmt 'Parse
                 -> NameResolutionState (Stmt 'NameRes)
resolveNamesStmt (ExprStmt s v)    = ExprStmt s <$> resolveNamesExpr v
resolveNamesStmt (Return s v)      = Return s <$> resolveNamesExpr v
resolveNamesStmt (VarDecl s n t v) = do
  addDef True n
  VarDecl s n t <$> resolveNamesExpr v

-- | Resolve names in expressions.
resolveNamesExpr :: Expr 'Parse
                 -> NameResolutionState (Expr 'NameRes)
resolveNamesExpr (Literal s v)    = pure $ Literal s v
resolveNamesExpr (Grouped s v)    = Grouped s <$> resolveNamesExpr v
resolveNamesExpr (BinOp s o l r)  = do
  l' <- resolveNamesExpr l
  r' <- resolveNamesExpr r
  pure $ BinOp s o l' r'
resolveNamesExpr (FuncCall s f p) = do
  f' <- resolveNamesExpr f
  p' <- traverse resolveNamesExpr p
  pure $ FuncCall s f' p'
resolveNamesExpr (VarRef s n)     = do
  scopes <- get
  let path = dropWhile (not . hasDef (lValue n)) scopes
  case path of
    [] -> tell [UndefinedReference n] >> pure (VarRef (s, Nothing) n)
    _  -> let np = map scopeName (reverse path) ++ [lValue n]
          in pure $ VarRef (s, Just np) n
