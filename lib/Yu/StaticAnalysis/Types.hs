{-|
Module      : Yu.StaticAnalysis.Types
Description : Static analysis and transformation types.

Types for static analysis and transformations.
-}

module Yu.StaticAnalysis.Types
  ( StaticAnalyzer
  , StaticAnalysisError(..)
  ) where

import Control.Monad.Writer.Strict
import Yu.Syntax.Parser

data StaticAnalysisError
  = Placeholder
  deriving (Show, Eq, Ord)

-- | Type for static analyzers.
type StaticAnalyzer (d1 :: Phase) (d2 :: Phase) = Module d1 -> Writer [StaticAnalysisError] (Module d1)
