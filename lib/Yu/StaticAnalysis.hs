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
import qualified Yu.StaticAnalysis.TypeCheck as TC

-- | Apply all static analysis operations and transformations.
applyAll :: Module 'Parse -> Writer [StaticAnalysisError] (Module 'TypeCheck)
applyAll = P.applyPrecedences >=> NR.resolveNames >=> TC.typeCheck
