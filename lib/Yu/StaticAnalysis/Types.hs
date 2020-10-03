{-|
Module      : Yu.StaticAnalysis.Types
Description : Static analysis and transformation types.

Types for static analysis and transformations.
-}

module Yu.StaticAnalysis.Types
  ( StaticAnalysisError(..)
  , NameResolutionError(..)
  , TypeAnalysisError(..)
  , analysisErrorErrata
  ) where

import           Prelude hiding (span)
import qualified Data.Text as T
import           Errata

import           Yu.Syntax.AST
import           Yu.Syntax.Span

-- | Errors that can occur during name resolution.
data NameResolutionError
  = UndefinedReference Identifier
  | DuplicateDefinition Identifier Identifier
  deriving (Show, Eq, Ord)

-- | Error that can occur during type analysis.
data TypeAnalysisError
  = MismatchedTypes YuType (Expr 'TypeCheck)
  | NotAFunction (Expr 'TypeCheck)
  | UnknownType Identifier
  deriving (Show, Eq, Ord)

-- | Union of all possible static analysis errors.
data StaticAnalysisError
  = NameResErr NameResolutionError
  | TypeAnalysisErr TypeAnalysisError
  deriving (Show, Eq, Ord)

-- | Turn analysis errors into Errata errors.
analysisErrorErrata :: StaticAnalysisError -> Errata
analysisErrorErrata (NameResErr e)      = nameResErr e
analysisErrorErrata (TypeAnalysisErr e) = typeErr e

-- | Turn name resolution errors into Errata errors.
nameResErr :: NameResolutionError -> Errata
nameResErr (UndefinedReference (Located (Span fp (l1, c1) (l2, c2)) n)) = errataSimple
  (Just $ red "error: reference to undefined symbol")
  (blockMerged'
    fancyRedStyle
    fp
    Nothing
    (l1, c1, Nothing)
    (l2, c2 - 1, Nothing)
    Nothing
    (Just $ "undefined symbol `" <> n <> "`")
  )
  Nothing
nameResErr (DuplicateDefinition i1 i2) =
  let (Located (Span fp (l11, c11) (_, c12)) _) = i1
      (Located (Span _  (l21, c21) (_, c22)) _) = i2
  in errataSimple
      (Just $ red "error: redefinition of symbol")
      (blockConnected
        fancyRedStyle
        fp
        Nothing
        (l21, c21, c22, Just $ "first definition here")
        (l11, c11, c12, Just $ "second definition here")
        Nothing
      )
      Nothing

-- | Get human redable name of type.
displayType :: YuType -> T.Text
displayType YuVoid        = "void"
displayType YuInt         = "int"
displayType YuBool        = "bool"
displayType YuUnknownType = "{unknown}"
displayType (YuFunc ps r) = "(" <> T.intercalate ", " (map displayType ps) <> ") -> " <> displayType r

-- | Turn type analysis errors into Errat errors.
typeErr :: TypeAnalysisError -> Errata
typeErr (MismatchedTypes t e) =
  let Span fp (l1, c1) (l2, c2) = span e
  in errataSimple
    (Just $ red "error: mismatched types")
    (blockMerged'
      fancyRedStyle
      fp
      Nothing
      (l1, c1, Nothing)
      (l2, c2 - 1, Nothing)
      Nothing
      (Just $ "expected type `" <> displayType t <> "` but found type `" <> displayType (getType e) <> "`")
    )
    Nothing
typeErr (NotAFunction e) =
  let Span fp (l1, c1) (l2, c2) = span e
  in errataSimple
    (Just $ red "error: attempting to call non-function")
    (blockMerged'
      fancyRedStyle
      fp
      Nothing
      (l1, c1, Nothing)
      (l2, c2 - 1, Nothing)
      Nothing
      Nothing
    )
    Nothing
typeErr (UnknownType (Located (Span fp (l1, c1) (l2, c2)) n)) = errataSimple
  (Just $ red "error: type of reference could not be determined")
  (blockMerged'
    fancyRedStyle
    fp
    Nothing
    (l1, c1, Nothing)
    (l2, c2 - 1, Nothing)
    Nothing
    (Just $ "reference to `" <> n <> "` has unknown type")
  )
  Nothing

red :: T.Text -> T.Text
red t = "\x1b[31m" <> t <> "\x1b[0m"
