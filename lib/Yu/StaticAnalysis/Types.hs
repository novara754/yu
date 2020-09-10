{-|
Module      : Yu.StaticAnalysis.Types
Description : Static analysis and transformation types.

Types for static analysis and transformations.
-}

module Yu.StaticAnalysis.Types
  ( StaticAnalyzer
  ) where

import Control.Monad.Writer.Strict
import Yu.Syntax.Parser
import Yu.Error

-- | Type for static analyzers.
type StaticAnalyzer (d1 :: Phase) (d2 :: Phase) = Module d1 -> Writer [CustomError] (Module d1)
