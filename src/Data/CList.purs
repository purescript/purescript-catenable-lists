module Data.CList
  ( CList(..)
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

import qualified Data.DList as D
import qualified Data.List as L

data CList a = CNil | CCons a (D.DList (CList a))

empty :: forall a. CList a
empty = CNil

null :: forall a. CList a -> Boolean
null CNil = true
null _ = false

append :: forall a. CList a -> CList a -> CList a
append as CNil = as
append CNil as = as
append as bs = link as bs

cons :: forall a. a -> CList a -> CList a
cons a clist = append (CCons a D.empty) clist

snoc :: forall a. CList a -> a -> CList a
snoc clist a = append clist (CCons a D.empty)

uncons :: forall a. CList a -> Maybe (Tuple a (CList a))
uncons CNil = Nothing
uncons (CCons a dlist) = Just (Tuple a (if D.null dlist
                                           then CNil
                                           else linkAll dlist))

link :: forall a. CList a -> CList a -> CList a
link CNil clist = clist
link (CCons a dlist) clist = CCons a (D.snoc dlist clist)

linkAll :: forall a. D.DList (CList a) -> CList a
linkAll dlist = foldr link CNil dlist

foldr :: forall a. (CList a -> CList a -> CList a) -> CList a -> D.DList (CList a) -> CList a
foldr k = foldr' (\a -> Left (\b -> k a b))

foldr' :: forall a. (CList a -> Either (CList a -> CList a) (CList a)) -> CList a -> D.DList (CList a) -> CList a
foldr' k b dlist = go dlist L.Nil
  where
  unroll :: CList a -> L.List (CList a -> CList a) -> CList a
  unroll = foldl (\x i -> i x)

  foldl :: forall a b. (b -> a -> b) -> b -> L.List a -> b
  foldl _ b L.Nil = b
  foldl k b (L.Cons a as) = foldl k (k b a) as

  go :: D.DList (CList a) -> L.List (CList a -> CList a) -> CList a
  go xs ys = case D.uncons xs of
                  Nothing -> unroll b ys
                  Just (Tuple a rest) ->
                    case k a of
                         Right b' -> unroll b' ys
                         Left j -> go rest (L.Cons j ys)

instance showCList :: (Show a) => Show (CList a) where
  show CNil = "CNil"
  show (CCons a as) = "CList (" ++ show a ++ ") (" ++ show as ++ ")"
