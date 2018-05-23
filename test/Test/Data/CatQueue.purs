module Test.Data.CatQueue (testCatQueue) where

import Data.CatQueue

import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Tuple (fst, snd)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, (==), ($), (<$>), (<<<), discard)
import Test.Assert (assert)

testCatQueue :: Effect Unit
testCatQueue = unsafePartial do
  log "null should be true for the empty queue"
  assert $ null empty

  log "singleton should create a queue with one element"
  assert $ (fst <$> uncons (singleton 1)) == Just 1
  assert $ (null <<< snd <$> uncons (singleton 1)) == Just true

  log "snoc should add an item to the end of the queue"
  assert $ fst (fromJust (uncons ((empty `snoc` 10) `snoc` 20))) == 10
  assert $ fst (fromJust (uncons (snd (fromJust (uncons ((empty `snoc` 10) `snoc` 20)))))) == 20

  log "uncons of the empty queue should be Nothing"
  assert $ isNothing (uncons empty)

  log "uncons of a queue with left and right lists should remove items properly"
  let queue1 = ((empty `snoc` 10) `snoc` 20) `snoc` 30
  assert $ fst (fromJust (uncons queue1)) == 10
  assert $ fst (fromJust (uncons (snd (fromJust (uncons queue1))))) == 20
  assert $ fst (fromJust (uncons (snd (fromJust (uncons (snd (fromJust (uncons queue1)))))))) == 30
