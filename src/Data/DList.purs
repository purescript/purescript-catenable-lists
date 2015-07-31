module Data.DList
  ( DList(..)
  , empty
  , null
  , snoc
  , uncons
  ) where

import Data.List (List(..), (:), reverse)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

data DList a = DList (List a) (List a)

empty :: forall a. DList a
empty = DList Nil Nil

null :: forall a. DList a -> Boolean
null (DList Nil Nil) = true
null _ = false

snoc :: forall a. a -> DList a -> DList a
snoc a (DList Nil r) = DList (reverse (a : r)) Nil
snoc a (DList l r) = DList l (a : r)

uncons :: forall a. DList a -> Maybe (Tuple a (DList a))
uncons (DList Nil Nil) = Nothing
uncons (DList Nil as) = uncons (DList (reverse as) Nil)
uncons (DList (Cons a Nil) as) = Just (Tuple a (DList (reverse as) Nil))
uncons (DList (Cons a as) bs) = Just (Tuple a (DList as bs))
