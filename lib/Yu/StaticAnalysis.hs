{-|
Module      : Yu.StaticAnalysis
Description : Static analysis and transformations.

Main module for static analysis and transformations.
-}

module Yu.StaticAnalysis
  ( applyAll
  ) where

import           Yu.StaticAnalysis.Types
import qualified Yu.StaticAnalysis.Precedences as P
import           Yu.Syntax.AST

-- | Apply all static analysis operations and transformations.
applyAll :: StaticAnalyzer 'Parse 'Parse
applyAll = P.applyPrecedences
