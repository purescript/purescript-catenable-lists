module Test.Data.CatQueue (testCatQueue) where

import Data.CatQueue
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Tuple (fst, snd)
import Data.Unfoldable (replicate)
import Partial.Unsafe (unsafePartial)
import Test.Assert (ASSERT, assert)

testCatQueue :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testCatQueue = unsafePartial do
  log "null should be true for the empty list"
  assert $ null empty

  log "singleton should create a queue with one element"
  assert $ (fst <$> uncons (singleton 1)) == Just 1
  assert $ (null <<< snd <$> uncons (singleton 1)) == Just true

  log "snoc should add an item to the end of the list"
  assert $ fst (fromJust (uncons ((empty `snoc` 10) `snoc` 20))) == 10
  assert $ fst (fromJust (uncons (snd (fromJust (uncons ((empty `snoc` 10) `snoc` 20)))))) == 20

  log "uncons of the empty list should be Nothing"
  assert $ isNothing (uncons empty)

  log "uncons of a list with left and right lists should remove items properly"
  let list1 = ((empty `snoc` 10) `snoc` 20) `snoc` 30
  assert $ fst (fromJust (uncons list1)) == 10
  assert $ fst (fromJust (uncons (snd (fromJust (uncons list1))))) == 20
  assert $ fst (fromJust (uncons (snd (fromJust (uncons (snd (fromJust (uncons list1)))))))) == 30

  log "appending two empty lists should be empty"
  assert $ null (empty <> empty)

  log "foldMap over a queue of monoids should produce the concatenation of the monoids"
  let queue2 = ((empty `snoc` "a") `snoc` "b") `snoc` "c"
  assert $ foldMap id queue2 == "abc"

  log "fromFoldable should convert an array into a CatList with the same values"
  let queue3 = fromFoldable ["a", "b", "c"]
  assert $ fst (fromJust (uncons queue3)) == "a"
  assert $ fst (fromJust (uncons (snd (fromJust (uncons queue3))))) == "b"
  assert $ fst (fromJust (uncons (snd (fromJust (uncons (snd (fromJust (uncons queue3)))))))) == "c"
  assert $ null (snd (fromJust (uncons (snd (fromJust (uncons (snd (fromJust (uncons queue3)))))))))

  log "functor should correctly map a function over the contents of a CatList"
  let queue4 = (_ + 3) <$> fromFoldable [1, 2, 3]
  assert $ foldMap (\v -> [v]) queue4 == [4, 5, 6]

  log "replicate should produce a CatList with a value repeated"
  let queue5 = (replicate 3 "foo") :: CatQueue String
  assert $ foldMap (\v -> [v]) queue5 == ["foo", "foo", "foo"]
