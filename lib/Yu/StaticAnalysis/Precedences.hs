{-|
Module      : Yu.StaticAnalysis.Precedences
Description : Operator precedence transformations.

Apply operator precedences to a freshly parsed Yu module.
-}

module Yu.StaticAnalysis.Precedences
  ( applyPrecedences
  ) where

import Control.Monad.Writer

import Yu.StaticAnalysis.Types
import Yu.StaticAnalysis.Precedences.Internal
import Yu.Syntax.Parser

-- | Apply operator precedences to a freshly parsed Yu module.
applyPrecedences :: Module 'Parse -> Writer [StaticAnalysisError] (Module 'Parse)
applyPrecedences (Module decls) = pure . Module $ map applyDecl decls
