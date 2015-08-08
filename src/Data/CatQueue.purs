-- | This module defines a strict queue.
module Data.CatQueue
  ( CatQueue(..)
  , empty
  , null
  , cons
  , snoc
  , uncons
  ) where

import Prelude (Show, (++), show)

import Data.List (List(..), (:), reverse)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

data CatQueue a = CatQueue (List a) (List a)

empty :: forall a. CatQueue a
empty = CatQueue Nil Nil

null :: forall a. CatQueue a -> Boolean
null (CatQueue Nil Nil) = true
null _ = false

cons :: forall a. a -> CatQueue a -> CatQueue a
cons a (CatQueue l r) = CatQueue (a : l) r

snoc :: forall a. CatQueue a -> a -> CatQueue a
snoc (CatQueue Nil r) a = CatQueue (reverse (a : r)) Nil
snoc (CatQueue l r) a = CatQueue l (a : r)

uncons :: forall a. CatQueue a -> Maybe (Tuple a (CatQueue a))
uncons (CatQueue Nil Nil) = Nothing
uncons (CatQueue Nil as) = uncons (CatQueue (reverse as) Nil)
uncons (CatQueue (Cons a Nil) as) = Just (Tuple a (CatQueue (reverse as) Nil))
uncons (CatQueue (Cons a as) bs) = Just (Tuple a (CatQueue as bs))

instance showCatQueue :: (Show a) => Show (CatQueue a) where
  show (CatQueue l r) = "CatQueue (" ++ show l ++ ") (" ++ show r ++ ")"
