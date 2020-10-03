{-|
Module      : Yu.StaticAnalysis.TypeCheck
Description : Perform type inference and type checking.

Perform type inference of expressions and verify against the types of holes
where they are used.
-}

module Yu.StaticAnalysis.TypeCheck
  ( typeCheck
  ) where

import qualified Data.Map as M
import           Control.Monad.Writer
import           Control.Monad.State

import           Yu.StaticAnalysis.Types
import           Yu.StaticAnalysis.TypeCheck.Internal
import           Yu.Syntax.AST

typeCheck :: Module 'NameRes -> Writer [StaticAnalysisError] (Module 'TypeCheck)
typeCheck m = mapWriterT f $ typeCheckMod m
  where
    f s =
      let preludeTypes = M.fromList
            [ (["+"], YuFunc [YuInt, YuInt] YuInt)
            , (["-"], YuFunc [YuInt, YuInt] YuInt)
            , (["*"], YuFunc [YuInt, YuInt] YuInt)
            , (["/"], YuFunc [YuInt, YuInt] YuInt)
            , (["%"], YuFunc [YuInt, YuInt] YuInt)
            , (["=="], YuFunc [YuInt, YuInt] YuBool)
            , (["/="], YuFunc [YuInt, YuInt] YuBool)
            , (["<"], YuFunc [YuInt, YuInt] YuBool)
            , ([">"], YuFunc [YuInt, YuInt] YuBool)
            , (["<="], YuFunc [YuInt, YuInt] YuBool)
            , ([">="], YuFunc [YuInt, YuInt] YuBool)
            ]
          (m', es) = evalState s preludeTypes
      in pure (m', map TypeAnalysisErr es)
