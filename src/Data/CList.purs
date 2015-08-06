module Data.CList
  ( CList(..)
  , empty
  , null
  , append
  , cons
  , snoc
  , uncons
  ) where

import Prelude (Monad, Show, (++), (>>=), pure, show)

import Control.Monad.Rec.Class (MonadRec)
import Control.Safely (Operator, safely)

import Data.Identity (Identity(..), runIdentity)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import qualified Data.DList as D

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
linkAll dlist = runIdentity (foldrS (\a b -> Identity (link a b)) CNil dlist)

foldrM :: forall m a. (Monad m) => (CList a -> CList a -> m (CList a)) -> CList a -> D.DList (CList a) -> m (CList a)
foldrM k b dlist = case D.uncons dlist of
                        Nothing -> pure CNil
                        Just (Tuple a as) -> (foldrM k b as) >>= k a

newtype Foldr m = Foldr (forall a.(CList a -> CList a -> m (CList a)) -> CList a -> D.DList (CList a) -> m (CList a))

instance operatorFoldr :: Operator Foldr where
  mapO to from (Foldr r) = Foldr \k b dlist -> to (r (\i j -> from (k i j)) b dlist)

runFoldr :: forall m a. Foldr m -> (CList a -> CList a -> m (CList a)) -> CList a -> D.DList (CList a) -> m (CList a)
runFoldr (Foldr r) = r

foldrS :: forall m a. (MonadRec m) => (CList a -> CList a -> m (CList a)) -> CList a -> D.DList (CList a) -> m (CList a)
foldrS = runFoldr (safely (Foldr foldrM))

instance showCList :: (Show a) => Show (CList a) where
  show CNil = "CNil"
  show (CCons a as) = "CList (" ++ show a ++ ") (" ++ show as ++ ")"
