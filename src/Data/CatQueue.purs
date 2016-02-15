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
  , snoc
  , uncons
  ) where

import Prelude (class Show, (++), show)

import Data.List (List(..), reverse)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

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

instance showCatQueue :: (Show a) => Show (CatQueue a) where
  show (CatQueue l r) = "CatQueue (" ++ show l ++ ") (" ++ show r ++ ")"
