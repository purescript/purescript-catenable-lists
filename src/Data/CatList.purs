-- | This module defines a strict catenable list
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

import Prelude (Show, (++), show)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import qualified Data.CatQueue as Q
import qualified Data.List as L

data CatList a = CatNil | CatCons a (Q.CatQueue (CatList a))

empty :: forall a. CatList a
empty = CatNil

null :: forall a. CatList a -> Boolean
null CatNil = true
null _ = false

append :: forall a. CatList a -> CatList a -> CatList a
append as CatNil = as
append CatNil as = as
append as bs = link as bs

cons :: forall a. a -> CatList a -> CatList a
cons a cat = append (CatCons a Q.empty) cat

snoc :: forall a. CatList a -> a -> CatList a
snoc cat a = append cat (CatCons a Q.empty)

uncons :: forall a. CatList a -> Maybe (Tuple a (CatList a))
uncons CatNil = Nothing
uncons (CatCons a q) = Just (Tuple a (if Q.null q then CatNil else linkAll q))

link :: forall a. CatList a -> CatList a -> CatList a
link CatNil cat = cat
link (CatCons a q) cat = CatCons a (Q.snoc q cat)

linkAll :: forall a. Q.CatQueue (CatList a) -> CatList a
linkAll q = foldr link CatNil q

foldr :: forall a. (CatList a -> CatList a -> CatList a) -> CatList a -> Q.CatQueue (CatList a) -> CatList a
foldr k = foldr' (\a -> Left (\b -> k a b))

foldr' :: forall a. (CatList a -> Either (CatList a -> CatList a) (CatList a)) -> CatList a -> Q.CatQueue (CatList a) -> CatList a
foldr' k b q = go q L.Nil
  where
  unroll :: CatList a -> L.List (CatList a -> CatList a) -> CatList a
  unroll = foldl (\x i -> i x)

  foldl :: forall a b. (b -> a -> b) -> b -> L.List a -> b
  foldl _ b L.Nil = b
  foldl k b (L.Cons a as) = foldl k (k b a) as

  go :: Q.CatQueue (CatList a) -> L.List (CatList a -> CatList a) -> CatList a
  go xs ys = case Q.uncons xs of
                  Nothing -> unroll b ys
                  Just (Tuple a rest) ->
                    case k a of
                         Right b' -> unroll b' ys
                         Left j -> go rest (L.Cons j ys)

instance showCatList :: (Show a) => Show (CatList a) where
  show CatNil = "CatNil"
  show (CatCons a as) = "CatList (" ++ show a ++ ") (" ++ show as ++ ")"
