-- | This module defines a strict catenable list.
-- |
-- | The implementation is based on a queue where all operations require
-- | `O(1)` amortized time.
-- |
-- | However, any single `uncons` operation may run in `O(n)` time.
-- |
-- | See [Purely Functional Data Structures](http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf) (Okasaki 1996)
module Data.CatList
  ( CatList(..)
  , empty
  , null
  , append
  , cons
  , snoc
  , uncons
  ) where

import Prelude (class Semigroup, class Show, (++), show)

import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Tuple (Tuple(..))

import Data.CatQueue as Q
import Data.List as L

-- | A strict catenable list.
-- |
-- | `CatList` may be empty, represented by `CatNil`.
-- |
-- | `CatList` may be non-empty, represented by `CatCons`. The `CatCons`
-- | data constructor takes the first element of the list and a queue of
-- | `CatList`.
data CatList a = CatNil | CatCons a (Q.CatQueue (CatList a))

-- | Create an empty catenable list.
-- |
-- | Running time: `O(1)`
empty :: forall a. CatList a
empty = CatNil

-- | Test whether a catenable list is empty.
-- |
-- | Running time: `O(1)`
null :: forall a. CatList a -> Boolean
null CatNil = true
null _ = false

-- | Append all elements of a catenable list to the end of another
-- | catenable list, create a new catenable list.
-- |
-- | Running time: `O(1)`
append :: forall a. CatList a -> CatList a -> CatList a
append as CatNil = as
append CatNil as = as
append as bs = link as bs

-- | Append an element to the beginning of the catenable list, creating a new
-- | catenable list.
-- |
-- | Running time: `O(1)`
cons :: forall a. a -> CatList a -> CatList a
cons a cat = append (CatCons a Q.empty) cat

-- | Append an element to the end of the catenable list, creating a new
-- | catenable list.
-- |
-- | Running time: `O(1)`
snoc :: forall a. CatList a -> a -> CatList a
snoc cat a = append cat (CatCons a Q.empty)

-- | Decompose a catenable list into a `Tuple` of the first element and
-- | the rest of the catenable list.
-- |
-- | Running time: `O(1)`
-- |
-- | Note that any single operation may run in `O(n)`.
uncons :: forall a. CatList a -> Maybe (Tuple a (CatList a))
uncons CatNil = Nothing
uncons (CatCons a q) = Just (Tuple a (if Q.null q then CatNil else (foldr link CatNil q)))

-- | Links two catenable lists by making appending the queue in the
-- | first catenable list to the second catenable list. This operation
-- | creates a new catenable list.
-- |
-- | Running time: `O(1)`
link :: forall a. CatList a -> CatList a -> CatList a
link CatNil cat = cat
link (CatCons a q) cat = CatCons a (Q.snoc q cat)

-- | Tail recursive version of foldr on `CatList`.
-- |
-- | Ensures foldl on `List` is tail-recursive.
foldr :: forall a. (CatList a -> CatList a -> CatList a) -> CatList a -> Q.CatQueue (CatList a) -> CatList a
foldr k b q = go q L.Nil
  where
  go :: Q.CatQueue (CatList a) -> L.List (CatList a -> CatList a) -> CatList a
  go xs ys = case Q.uncons xs of
                  Nothing -> foldl (\x i -> i x) b ys
                  Just (Tuple a rest) -> go rest (L.Cons (k a) ys)

  foldl :: forall b c. (c -> b -> c) -> c -> L.List b -> c
  foldl _ c L.Nil = c
  foldl k c (L.Cons b as) = foldl k (k c b) as

instance semigroupCatList :: Semigroup (CatList a) where
  append = append

instance monoidCatList :: Monoid (CatList a) where
  mempty = CatNil

instance showCatList :: (Show a) => Show (CatList a) where
  show CatNil = "CatNil"
  show (CatCons a as) = "CatList (" ++ show a ++ ") (" ++ show as ++ ")"
