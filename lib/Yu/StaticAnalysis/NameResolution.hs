{-|
Module      : Yu.StaticAnalysis.NameResolution
Description : Name resolution of references.

Perform name resolution of references to attach
the location of definitions for variable, function, etc. references.
-}

module Yu.StaticAnalysis.NameResolution
  ( resolveNames
  ) where

import Control.Monad.Writer
import Control.Monad.State

import Yu.Syntax.AST
import Yu.StaticAnalysis.Types
import Yu.StaticAnalysis.NameResolution.Internal

-- | Attach fully qualified names to variable references.
resolveNames :: Module 'Parse
             -> Writer [StaticAnalysisError] (Module 'NameRes)
resolveNames m = mapWriterT f $ resolveNamesModule m
  where
    f s =
      let (m', es) = evalState s []
      in pure (m', map NameResErr es)
