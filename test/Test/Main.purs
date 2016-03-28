module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Assert (ASSERT)
import Test.Data.CatList (testCatList)
import Test.Data.CatQueue (testCatQueue)

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  log "CatQueue"
  testCatQueue

  log ""

  log "CatList"
  testCatList
