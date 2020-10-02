{-|
Module      : Yu.StaticAnalysis.Types
Description : Static analysis and transformation types.

Types for static analysis and transformations.
-}

module Yu.StaticAnalysis.Types
  ( StaticAnalysisError(..)
  , NameResolutionError(..)
  , analysisErrorErrata
  ) where

import qualified Data.Text as T
import           Errata

import           Yu.Syntax.AST
import           Yu.Syntax.Span

-- | Errors that can occur during name resolution.
data NameResolutionError
  = UndefinedReference Identifier
  | DuplicateDefinition Identifier Identifier
  deriving (Show, Eq, Ord)

-- | Union of all possible static analysis errors.
data StaticAnalysisError
  = NameResErr NameResolutionError
  deriving (Show, Eq, Ord)

-- | Turn analysis errors into Errata errors.
analysisErrorErrata :: StaticAnalysisError -> Errata
analysisErrorErrata (NameResErr e) = nameResErr e

-- | Turn name resolution errors into Errata errors.
nameResErr :: NameResolutionError -> Errata
nameResErr (UndefinedReference (Located (Span fp _ (l1, c1) (l2, c2)) n)) = errataSimple
  (Just $ red "error: reference to undefined symbol")
  (blockMerged'
    fancyRedStyle
    fp
    (l1, c1, Nothing)
    (l2, c2 - 1, Nothing)
    Nothing
    (Just $ "undefined symbol `" <> n <> "`")
  )
  Nothing
nameResErr (DuplicateDefinition i1 i2) =
  let (Located (Span fp _ (l11, c11) (_, c12)) _) = i1
      (Located (Span _  _ (l21, c21) (_, c22)) _) = i2
  in errataSimple
      (Just $ red "error: redefinition of symbol")
      (blockConnected
        fancyRedStyle
        fp
        (l21, c21, c22, Just $ "first definition here")
        (l11, c11, c12, Just $ "second definition here")
        Nothing
      )
      Nothing

red :: T.Text -> T.Text
red t = "\x1b[31m" <> t <> "\x1b[0m"
