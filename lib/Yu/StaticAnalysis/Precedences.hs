{-|
Module      : Yu.StaticAnalysis.Precedences
Description : Operator precedence transformations.

Apply operator precedences to a freshly parsed Yu module.
-}

module Yu.StaticAnalysis.Precedences
  ( applyPrecedences
  ) where

import           Yu.StaticAnalysis.Types
import           Yu.StaticAnalysis.Precedences.Internal
import           Yu.Syntax.Parser

-- | Apply operator precedences to a freshly parsed Yu module.
applyPrecedences :: StaticAnalyzer
applyPrecedences (Module decls) = pure . Module $ map (fmap applyDecl) decls
