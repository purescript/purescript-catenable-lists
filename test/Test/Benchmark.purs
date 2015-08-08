module Test.Benchmark (runBenchmarks) where

import Prelude

import Control.Monad.Eff

import Data.Foldable
import Data.Maybe
import Data.Tuple

import Benchotron.Core
import Benchotron.UI.Console

import Test.QuickCheck.Gen

import qualified Data.Array as A
import qualified Data.CatQueue as Q
import qualified Data.List as L

(..) = A.(..)

snocBenchmark :: forall eff. Benchmark eff
snocBenchmark = mkBenchmark
  { slug: "snoc"
  , title: "Add elements to the end of the queue"
  , sizes: (1..10) <#> (*100)
  , sizeInterpretation: "Number of elements to be added"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "CatQueue" (foldl Q.snoc Q.empty)
               , benchFn "List" (foldl L.snoc L.Nil)
               , benchFn "Array" (foldl A.snoc [])
               ]
  }

unconsBenchmark :: forall eff. Benchmark eff
unconsBenchmark = mkBenchmark
  { slug: "uncons"
  , title: "Remove elements from the front of the queue"
  , sizes: (1..10) <#> (*100)
  , sizeInterpretation: "Number of elements to be removed"
  , inputsPerSize: 1
  , gen: \n -> { queue: _, list: _, array: _ } <$> (randomCatQueue n) <*> (randomList n) <*> (randomArray n)
  , functions: [ benchFn "CatQueue" \a -> whileUncons isJust (\(Just (Tuple _ as)) -> as) Q.uncons a.queue
               , benchFn "List" \a -> whileUncons isJust (\(Just x) -> x.tail) L.uncons a.list
               , benchFn "Array" \a -> whileUncons isJust (\(Just x) -> x.tail) A.uncons a.array
               ]
  }

runBenchmarks = runSuite [ snocBenchmark, unconsBenchmark ]

randomCatQueue :: forall eff. Int -> Eff (BenchEffects eff) (Q.CatQueue Number)
randomCatQueue n = (foldl Q.snoc Q.empty) <$> (randomArray n)

randomList :: forall eff. Int -> Eff (BenchEffects eff) (L.List Number)
randomList n = (foldl L.snoc L.Nil) <$> (randomArray n)

foreign import randomArray :: forall eff. Int -> Eff (BenchEffects eff) (Array Number)

foreign import whileUncons :: forall list value uncons. (uncons -> Boolean) ->
                                                        (uncons -> list value) ->
                                                        (list value -> uncons) ->
                                                        list value ->
                                                        Unit
