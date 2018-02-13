-- | This module defines a strict queue.
-- |
-- | The queue implementation is based on a pair of lists where all
-- | operations require `O(1)` amortized time.
-- |
-- | However, any single `uncons` operation may run in `O(n)` time.
-- |
-- | See [Simple and Efficient Purely Functional Queues and Dequeues](http://www.westpoint.edu/eecs/SiteAssets/SitePages/Faculty%20Publication%20Documents/Okasaki/jfp95queue.pdf) (Okasaki 1995)
module Data.CatQueue
  ( CatQueue(..)
  , empty
  , null
  , singleton
  , append
  , snoc
  , uncons
  , fromFoldable
  ) where

import Prelude hiding (append)

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Foldable (class Foldable, foldMap, foldMapDefaultL, foldl, foldrDefault)
import Data.List (List(..), reverse)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)

-- | A strict queue representated using a pair of lists.
data CatQueue a = CatQueue (List a) (List a)

-- | Create an empty queue.
-- |
-- | Running time: `O(1)`
empty :: forall a. CatQueue a
empty = CatQueue Nil Nil

-- | Test whether a queue is empty.
-- |
-- | Running time: `O(1)`
null :: forall a. CatQueue a -> Boolean
null (CatQueue Nil Nil) = true
null _ = false

-- | Append all elements of a queue to the end of another
-- | queue, creating a new queue.
-- |
-- | Running time: `O(n) in the length of the second queue`
append :: forall a. CatQueue a -> CatQueue a -> CatQueue a
append cq = foldl snoc cq

-- | Create a queue containing a single element.
-- |
-- | Running time: `O(1)`
singleton :: forall a. a -> CatQueue a
singleton = snoc empty

-- | Append an element to the end of the queue, creating a new queue.
-- |
-- | Running time: `O(1)`
snoc :: forall a. CatQueue a -> a -> CatQueue a
snoc (CatQueue l r) a = CatQueue l (Cons a r)

-- | Decompose a queue into a `Tuple` of the first element and the rest of the queue.
-- |
-- | Running time: `O(1)`
-- |
-- | Note that any single operation may run in `O(n)`.
uncons :: forall a. CatQueue a -> Maybe (Tuple a (CatQueue a))
uncons (CatQueue Nil Nil) = Nothing
uncons (CatQueue Nil r) = uncons (CatQueue (reverse r) Nil)
uncons (CatQueue (Cons a as) r) = Just (Tuple a (CatQueue as r))

-- | Convert any `Foldable` into a `CatQueue`.
-- |
-- | Running time: `O(n)`
fromFoldable :: forall f a. Foldable f => f a -> CatQueue a
fromFoldable f = foldMap singleton f

cqEq :: forall a. Eq a => CatQueue a -> CatQueue a -> Boolean
cqEq = go
  where
    elemEq = eq :: (a -> a -> Boolean)
    go xs ys = case uncons xs, uncons ys of
      Just (Tuple x xs'), Just (Tuple y ys')
        | x `elemEq` y -> go xs' ys'
      Nothing, Nothing -> true
      _      , _       -> false

cqCompare :: forall a. Ord a => CatQueue a -> CatQueue a -> Ordering
cqCompare = go
  where
    elemCompare = compare :: (a -> a -> Ordering)
    go xs ys = case uncons xs, uncons ys of
      Just (Tuple x xs'), Just (Tuple y ys') ->
        case elemCompare x y of
             EQ       -> go xs' ys'
             ordering -> ordering
      Just _,   Nothing -> GT
      Nothing,  Just _  -> LT
      Nothing,  Nothing -> EQ

instance eqCatQueue :: Eq a => Eq (CatQueue a) where
  eq = cqEq

instance ordCatQueue :: Ord a => Ord (CatQueue a) where
  compare = cqCompare

instance semigroupCatQueue :: Semigroup (CatQueue a) where
  append = append

instance monoidCatQueue :: Monoid (CatQueue a) where
  mempty = empty

instance showCatQueue :: Show a => Show (CatQueue a) where
  show (CatQueue l r) = "(CatQueue " <> show l <> " " <> show r <> ")"

instance foldableCatQueue :: Foldable CatQueue where
  foldMap f q = foldMapDefaultL f q
  foldr f s q = foldrDefault f s q
  foldl f = go
    where
    go acc q = case uncons q of
       Just (Tuple x xs) -> go (f acc x) xs
       Nothing -> acc

instance unfoldableCatQueue :: Unfoldable CatQueue where
  unfoldr f b = go b empty
    where
      go source memo = case f source of
        Nothing -> memo
        Just (Tuple one rest) -> go rest (snoc memo one)

instance traversableCatQueue :: Traversable CatQueue where
  traverse f =
    map (foldl snoc empty)
    <<< foldl (\acc -> lift2 snoc acc <<< f) (pure empty)
  sequence = sequenceDefault

instance functorCatQueue :: Functor CatQueue where
  map f (CatQueue l r) = CatQueue (map f l) (map f r)

instance applyCatQueue :: Apply CatQueue where
  apply = ap

instance applicativeCatQueue :: Applicative CatQueue where
  pure = singleton

instance bindCatQueue :: Bind CatQueue where
  bind = flip foldMap

instance monadCatQueue :: Monad CatQueue

instance altCatQueue :: Alt CatQueue where
  alt = append

instance plusCatQueue :: Plus CatQueue where
  empty = empty

instance alternativeCatQueue :: Alternative CatQueue

instance monadZeroCatQueue :: MonadZero CatQueue

instance monadPlusCatQueue :: MonadPlus CatQueue
