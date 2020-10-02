{-|
Module      : Yu.Syntax.Span
Description : Location data.

This module contains types and functions to work with location data
of Yu language items: spans of tokens, their lines and columns, etc..
-}
module Yu.Syntax.Span
 ( HasSpan(..)
 , Located(..)
 , Span(..)
 , spanPos
 ) where

import Hectoparsec.Lexer

class HasSpan l where
  span :: l -> Span

-- | A located item.
data Located a = Located
  { lSpan  :: Span
  , lValue :: a
  } deriving (Eq, Ord)

instance Show a => Show (Located a) where
  showsPrec p (Located (Span _ _ (l1, c1) (l2, c2)) x) = showParen (p >= 11) (showString s)
    where
      s = show l1 <> ":" <> show c1 <> "-" <> show l2 <> ":" <> show c2 <> " " <> show x

instance Functor Located where
  fmap f (Located s x) = Located s $ f x

instance HasSpan (Located a) where
  span = lSpan

-- | Location and span of a located item.
data Span = Span
  { spanFile    :: FilePath   -- ^ Filepath.
  , spanOffsets :: (Int, Int) -- ^ Start and end of the span in the input stream.
  , spanStart   :: (Int, Int) -- ^ Starting line:column.
  , spanEnd     :: (Int, Int) -- ^ Ending line:column.
  } deriving (Show, Eq, Ord)

instance Semigroup Span where
    Span fp (os1, oe1) ss1 se1 <> Span _ (os2, oe2) ss2 se2
        = Span
            fp
            (min os1 os2, max oe1 oe2)
            (if min os1 os2 == os1 then ss1 else ss2)
            (if max oe1 oe2 == oe1 then se1 else se2)

-- | Construct a span from two positions.
spanPos :: Pos -> Pos -> Span
spanPos (Pos f os ls cs) (Pos _ oe le ce) = Span f (os, oe) (ls, cs) (le, ce)
