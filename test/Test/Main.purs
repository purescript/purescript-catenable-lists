module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)

import Test.Data.CatList
import Test.Data.CatQueue

main = do
  log "CatQueue"
  testCatQueue

  log ""

  log "CatList"
  testCatList
