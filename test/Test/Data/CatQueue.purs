module Test.Data.CatQueue (testCatQueue) where

import Data.CatQueue

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Tuple (fst, snd)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, (==), ($), (<$>), (<<<), discard)
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
