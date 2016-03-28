module Test.Data.CatList (testCatList) where

import Prelude (Unit, (==), ($), bind)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.CatList
import Data.Maybe (fromJust)
import Data.Tuple (fst, snd)

import Partial.Unsafe (unsafePartial)

import Test.Assert (ASSERT, assert)

testCatList :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testCatList = unsafePartial do
  log "null should be true for the empty list"
  assert $ null empty

  log "cons should add an item to the beginning of the list"
  assert $ fst (fromJust (uncons (20 `cons` (10 `cons` empty)))) == 20
  assert $ fst (fromJust (uncons (snd (fromJust (uncons (20 `cons` (10 `cons` empty))))))) == 10

  log "snoc should add an item to the end of the list"
  assert $ fst (fromJust (uncons ((empty `snoc` 10) `snoc` 20))) == 10
  assert $ fst (fromJust (uncons (snd (fromJust (uncons ((empty `snoc` 10) `snoc` 20)))))) == 20

  log "appending two empty lists should be empty"
  assert $ null (append empty empty)

  log "uncons of a list with left and right lists should remove items properly"
  let list1 = ((10 `cons` empty) `snoc` 20) `snoc` 30
  assert $ fst (fromJust (uncons list1)) == 10
  assert $ fst (fromJust (uncons (snd (fromJust (uncons list1))))) == 20
  assert $ fst (fromJust (uncons (snd (fromJust (uncons (snd (fromJust (uncons list1)))))))) == 30
