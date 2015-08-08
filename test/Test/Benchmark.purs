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
import qualified Data.CatList as C
import qualified Data.List as L
import qualified Data.Sequence as S

(..) = A.(..)

consBenchmark :: forall eff. Benchmark eff
consBenchmark = mkBenchmark
  { slug: "cons"
  , title: "Add elements to the beginning of the lists"
  , sizes: (1..10) <#> (*100)
  , sizeInterpretation: "Number of elements to be added"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "CatList" (foldr C.cons C.empty)
               , benchFn "List" (foldr L.(:) L.Nil)
               , benchFn "Sequence" (foldr S.cons S.empty)
               ]
  }

snocBenchmark :: forall eff. Benchmark eff
snocBenchmark = mkBenchmark
  { slug: "snoc"
  , title: "Add elements to the end of the lists"
  , sizes: (1..10) <#> (*100)
  , sizeInterpretation: "Number of elements to be added"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "CatQueue" (foldl Q.snoc Q.empty)
               , benchFn "CatList" (foldl C.snoc C.empty)
               , benchFn "List" (foldl L.snoc L.Nil)
               , benchFn "Sequence" (foldl S.snoc S.empty)
               ]
  }

unconsBenchmark :: forall eff. Benchmark eff
unconsBenchmark = mkBenchmark
  { slug: "uncons"
  , title: "Remove elements from the front of the list"
  , sizes: (1..10) <#> (*100)
  , sizeInterpretation: "Number of elements to be removed"
  , inputsPerSize: 1
  , gen: \n -> { queue: _
               , cat: _
               , list: _
               , sequence: _
               } <$> (randomCatQueue n)
                 <*> (randomCatList n)
                 <*> (randomList n)
                 <*> (randomSequence n)
  , functions: [ benchFn "CatQueue" \a -> whileUncons isJust (\(Just (Tuple _ x)) -> x) Q.uncons a.queue
               , benchFn "CatList" \a -> whileUncons isJust (\(Just (Tuple _ x)) -> x) C.uncons a.cat
               , benchFn "List" \a -> whileUncons isJust (\(Just x) -> x.tail) L.uncons a.list
               , benchFn "Sequence" \a -> whileUncons isJust (\(Just (Tuple _ x)) -> x) S.uncons a.sequence
               ]
  }

appendBenchmark :: forall eff. Benchmark eff
appendBenchmark = mkBenchmark
  { slug: "append"
  , title: "Add all elements from one list to the end of another list"
  , sizes: (1..10) <#> (*100)
  , sizeInterpretation: "Number of elements in the list"
  , inputsPerSize: 1
  , gen: \n -> { cat: _
               , list: _
               , sequence: _
               } <$> (randomCatList n)
                 <*> (randomList n)
                 <*> (randomSequence n)
  , functions: [ benchFn "CatList" \a -> C.append a.cat a.cat
               , benchFn "List" \a -> a.list <> a.list
               , benchFn "Sequence" \a -> S.append a.sequence a.sequence
               ]
  }

randomCatQueue :: forall eff. Int -> Eff (BenchEffects eff) (Q.CatQueue Number)
randomCatQueue n = (foldl Q.snoc Q.empty) <$> (randomArray n)

randomCatList :: forall eff. Int -> Eff (BenchEffects eff) (C.CatList Number)
randomCatList n = (foldl C.snoc C.empty) <$> (randomArray n)

randomList :: forall eff. Int -> Eff (BenchEffects eff) (L.List Number)
randomList n = (foldl L.snoc L.Nil) <$> (randomArray n)

randomSequence :: forall eff. Int -> Eff (BenchEffects eff) (S.Seq Number)
randomSequence n = (foldl S.snoc S.empty) <$> (randomArray n)

foreign import randomArray :: forall eff. Int -> Eff (BenchEffects eff) (Array Number)

foreign import whileUncons :: forall f a uncons. (uncons -> Boolean) -> (uncons -> f a) -> (f a -> uncons) -> f a -> Unit

runBenchmarks = runSuite [ consBenchmark
                         , snocBenchmark
                         , unconsBenchmark
                         , appendBenchmark
                         ]
