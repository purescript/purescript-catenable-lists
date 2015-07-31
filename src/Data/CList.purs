module Data.CList
  ( CList(..)
  , empty
  , append
  , snoc
  , uncons
  ) where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import qualified Data.DList as D

data CList a = CNil | CCons a (D.DList (CList a))

empty :: forall a. CList a
empty = CNil

append :: forall a. CList a -> CList a -> CList a
append as CNil = as
append CNil as = as
append as bs = link as bs

snoc :: forall a. CList a -> a -> CList a
snoc clist a = append clist (CCons a D.empty)

uncons :: forall a. CList a -> Maybe (Tuple a (CList a))
uncons CNil = Nothing
uncons (CCons a dlist) = Just (Tuple a (if D.null dlist
                                           then CNil
                                           else linkAll dlist))

link :: forall a. CList a -> CList a -> CList a
link (CCons a dlist) clist = CCons a (D.snoc clist dlist)

linkAll :: forall a. D.DList (CList a ) -> CList a
linkAll dlist = case D.uncons dlist of
                     Nothing -> CNil
                     Just (Tuple a as) -> if D.null as
                                             then a
                                             else link a (linkAll as)
