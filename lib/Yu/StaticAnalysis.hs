{-|
Module      : Yu.StaticAnalysis
Description : Static analysis and transformations.

Main module for static analysis and transformations.
-}

module Yu.StaticAnalysis
  ( module Yu.StaticAnalysis.Types
  , applyAll
  ) where

import           Control.Monad.Writer

import           Yu.StaticAnalysis.Types
import           Yu.Syntax.AST
import qualified Yu.StaticAnalysis.Precedences as P
import qualified Yu.StaticAnalysis.NameResolution as NR

-- | Apply all static analysis operations and transformations.
applyAll :: Module 'Parse -> Writer [StaticAnalysisError] (Module 'NameRes)
applyAll = P.applyPrecedences >=> NR.resolveNames
