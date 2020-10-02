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

import Hectoparsec

class HasSpan l where
  span :: l -> Span

-- | A located item.
data Located a = Located
  { lSpan  :: Span
  , lValue :: a
  } deriving (Eq, Ord)

instance Show a => Show (Located a) where
  showsPrec p (Located (Span _ (l1, c1) (l2, c2)) x) = showParen (p >= 11) (showString s)
    where
      s = show l1 <> ":" <> show c1 <> "-" <> show l2 <> ":" <> show c2 <> " " <> show x

instance Functor Located where
  fmap f (Located s x) = Located s $ f x

instance HasSpan (Located a) where
  span = lSpan

-- | Location and span of a located item.
data Span = Span
  { spanFile    :: FilePath   -- ^ Filepath.
  , spanStart   :: (Int, Int) -- ^ Starting line:column.
  , spanEnd     :: (Int, Int) -- ^ Ending line:column.
  } deriving (Show, Eq, Ord)

instance Semigroup Span where
  Span fp s1 e1 <> Span _ s2 e2 = Span fp (min s1 s2) (max e1 e2)

-- | Construct a span from two positions.
spanPos :: Pos -> Pos -> Span
spanPos (Pos f ls cs) (Pos _ le ce) = Span f (ls, cs) (le, ce)
