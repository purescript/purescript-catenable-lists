module Test.Data.DList (testDList) where

import Prelude

import Control.Monad.Eff.Console (log)

import Data.DList
import Data.Maybe (Maybe(..), isNothing)
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (fst, snd)

import Test.Assert (assert)

testDList = do
  log "null should be true for the empty list"
  assert $ null empty

  log "cons should add an item to the beginning of the list"
  assert $ fst (fromJust (uncons (20 `cons` (10 `cons` empty)))) == 20
  assert $ fst (fromJust (uncons (snd (fromJust (uncons (20 `cons` (10 `cons` empty))))))) == 10

  log "snoc should add an item to the end of the list"
  assert $ fst (fromJust (uncons ((empty `snoc` 10) `snoc` 20))) == 10
  assert $ fst (fromJust (uncons (snd (fromJust (uncons ((empty `snoc` 10) `snoc` 20)))))) == 20

  log "uncons of the empty list should be Nothing"
  assert $ isNothing (uncons empty)

  log "uncons of a list with left and right lists should remove items properly"
  let list1 = ((10 `cons` empty) `snoc` 20) `snoc` 30
  assert $ fst (fromJust (uncons list1)) == 10
  assert $ fst (fromJust (uncons (snd (fromJust (uncons list1))))) == 20
  assert $ fst (fromJust (uncons (snd (fromJust (uncons (snd (fromJust (uncons list1)))))))) == 30
