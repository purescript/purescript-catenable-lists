module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)

import Test.Data.CList
import Test.Data.DList

main = do
  log "DList -----------------------------------------"
  testDList

  log "CList -----------------------------------------"
  testCList
