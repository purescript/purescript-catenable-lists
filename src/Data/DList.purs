module Data.DList
  ( DList(..)
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

data DList a = DList (List a) (List a)

empty :: forall a. DList a
empty = DList Nil Nil

null :: forall a. DList a -> Boolean
null (DList Nil Nil) = true
null _ = false

cons :: forall a. a -> DList a -> DList a
cons a (DList l r) = DList (a : l) r

snoc :: forall a. DList a -> a -> DList a
snoc (DList Nil r) a = DList (reverse (a : r)) Nil
snoc (DList l r) a = DList l (a : r)

uncons :: forall a. DList a -> Maybe (Tuple a (DList a))
uncons (DList Nil Nil) = Nothing
uncons (DList Nil as) = uncons (DList (reverse as) Nil)
uncons (DList (Cons a Nil) as) = Just (Tuple a (DList (reverse as) Nil))
uncons (DList (Cons a as) bs) = Just (Tuple a (DList as bs))

instance showDList :: (Show a) => Show (DList a) where
  show (DList l r) = "DList (" ++ show l ++ ") (" ++ show r ++ ")"
