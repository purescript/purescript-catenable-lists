## Module Data.CatQueue

This module defines a strict queue.

The queue implementation is based on a pair of lists where all
operations require `O(1)` amortized time.

However, any single `uncons` operation may run in `O(n)` time.

See [Simple and Efficient Purely Functional Queues and Dequeues](http://www.westpoint.edu/eecs/SiteAssets/SitePages/Faculty%20Publication%20Documents/Okasaki/jfp95queue.pdf) (Okasaki 1995)

#### `CatQueue`

``` purescript
data CatQueue a
  = CatQueue (List a) (List a)
```

A strict queue representated using a pair of lists.

##### Instances
``` purescript
instance showCatQueue :: (Show a) => Show (CatQueue a)
```

#### `empty`

``` purescript
empty :: forall a. CatQueue a
```

Create an empty queue.

Running time: `O(1)`

#### `null`

``` purescript
null :: forall a. CatQueue a -> Boolean
```

Test whether a queue is empty.

Running time: `O(1)`

#### `snoc`

``` purescript
snoc :: forall a. CatQueue a -> a -> CatQueue a
```

Append an element to the end of the queue, creating a new queue.

Running time: `O(1)`

#### `uncons`

``` purescript
uncons :: forall a. CatQueue a -> Maybe (Tuple a (CatQueue a))
```

Decompose a queue into a `Tuple` of the first element and the rest of the queue.

Running time: `O(1)`

Note that any single operation may run in `O(n)`.


