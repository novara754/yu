{-|
Module      : Yu.StaticAnalysis.TypeCheck.Internal
Description : Perform type inference and type checking.

Perform type inference of expressions and verify against the types of holes
where they are used.

This module is internal and may break between non-major versions.
-}

module Yu.StaticAnalysis.TypeCheck.Internal
  ( typeCheckMod
  , typeCheckDecl
  , inferTypeExpr
  , getType
  ) where

import           Data.Foldable
import           Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import           Control.Monad.Writer
import           Control.Monad.State

import           Yu.Syntax.Span
import           Yu.Syntax.AST
import           Yu.StaticAnalysis.Types

-- | Storage for types of variables and functions.
type TypeDB = M.Map NamePath YuType

-- | State of the type inferer, storing types of variables and functions.
type TypeInferenceState = WriterT [TypeAnalysisError] (State TypeDB)

-- | Get type of reference from db.
getRefType :: NamePath -> TypeInferenceState YuType
getRefType n = fromMaybe YuUnknownType . M.lookup n <$> get

-- | Add new reference type to db.
addRefType :: NamePath -> YuType -> TypeInferenceState ()
addRefType n t = modify $ M.insert n t

-- | Check if type of expression matches expected type.
matchesType :: YuType -> (Expr 'TypeCheck) -> Bool
matchesType t e = t == getType e

-- | Checks type of expression against given type, reports error when they differ.
ensureType :: YuType -> (Expr 'TypeCheck) -> TypeInferenceState ()
ensureType t e = unless (matchesType t e) $ tell [MismatchedTypes t e]

-- | Turn identifier into type.
identToType :: T.Text -> YuType
identToType "int"  = YuInt
identToType "bool" = YuBool
identToType _      = YuUnknownType

-- | Helper function to "move" Param type onto the next phase.
updateParam :: Param 'NameRes -> Param 'TypeCheck
updateParam (Param a b c) = Param a b c

-- | Check types in a module.
typeCheckMod :: Module 'NameRes -> TypeInferenceState (Module 'TypeCheck)
typeCheckMod (Module ds) = do
  modify $ (<> M.fromList (mapMaybe declType ds))
  ds' <- traverse typeCheckDecl ds
  pure $ Module ds'
  where
    declType (FunctionDecl (_, n) _ ps rt _) =
      let ps' = map (identToType . lValue . paramType) ps
          rt' = fromMaybe YuVoid $ identToType . lValue <$> rt
      in Just $ (n, YuFunc ps' rt')
    declType _ = Nothing

-- | Check types in declaration.
typeCheckDecl :: Decl 'NameRes -> TypeInferenceState (Decl 'TypeCheck)
typeCheckDecl (ModuleDecl s n)         = pure $ ModuleDecl s n
typeCheckDecl (FunctionDecl s i p r b) = do
  let r' = fromMaybe YuVoid $ identToType . lValue <$> r
  traverse_ addParam p
  b' <- traverse (typeCheckStmt r') b
  pure $ FunctionDecl s i (map updateParam p) r b'
  where
    addParam (Param (_, n) _ t) = addRefType n (identToType $ lValue t)

-- | Check types in statement.
typeCheckStmt :: YuType -> Stmt 'NameRes -> TypeInferenceState (Stmt 'TypeCheck)
typeCheckStmt rt = \case
  VarDecl (s, n) i t e -> do
    e' <- inferTypeExpr e
    case identToType . lValue <$> t of
      Just t' -> do
        ensureType t' e'
        addRefType n t'
      Nothing -> do
        addRefType n (getType e')
    pure $ VarDecl (s, n) i t e'
  ExprStmt s e -> ExprStmt s <$> inferTypeExpr e
  Return s e -> do
    e' <- inferTypeExpr e
    ensureType rt e'
    pure $ Return s e'

-- | Infer type of expression.
inferTypeExpr :: Expr 'NameRes -> TypeInferenceState (Expr 'TypeCheck)
inferTypeExpr (Literal s v) =
  let t = case v of
        LiteralInt _  -> YuInt
        LiteralBool _ -> YuBool
  in pure $ Literal (s, t) v
inferTypeExpr (VarRef (s, n) i) = do
  t <- case n of
    Just n' -> getRefType n'
    Nothing -> pure YuUnknownType
  when (t == YuUnknownType) $ tell [UnknownType i]
  pure $ VarRef (s, n, t) i
inferTypeExpr (BinOp s o l r) = do
  l' <- inferTypeExpr l
  r' <- inferTypeExpr r
  ot <- getRefType [lValue o]
  case ot of
    YuFunc [el, er] t -> do
      ensureType el l'
      ensureType er r'
      pure $ BinOp (s, t) o l' r'
    _ ->
      pure $ BinOp (s, YuUnknownType) o l' r'
inferTypeExpr (FuncCall s f ps) = do
  f' <- inferTypeExpr f
  ps' <- traverse inferTypeExpr ps
  case getType f' of
    YuFunc pts rt -> do
      traverse_ (uncurry ensureType) $ zip pts ps'
      pure $ FuncCall (s, rt) f' ps'
    _ -> do
      tell [NotAFunction f']
      pure $ FuncCall (s, YuUnknownType) f' ps'
inferTypeExpr (Grouped s e) = do
  e' <- inferTypeExpr e
  pure $ Grouped (s, getType e') e'
