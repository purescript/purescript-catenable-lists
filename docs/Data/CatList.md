## Module Data.CatList

This module defines a strict catenable list

See [Purely Functional Data Structures](http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf) (Okasaki 1996)

#### `CatList`

``` purescript
data CatList a
  = CatNil
  | CatCons a (CatQueue (CatList a))
```

##### Instances
``` purescript
instance showCatList :: (Show a) => Show (CatList a)
```

#### `empty`

``` purescript
empty :: forall a. CatList a
```

#### `null`

``` purescript
null :: forall a. CatList a -> Boolean
```

#### `append`

``` purescript
append :: forall a. CatList a -> CatList a -> CatList a
```

#### `cons`

``` purescript
cons :: forall a. a -> CatList a -> CatList a
```

#### `snoc`

``` purescript
snoc :: forall a. CatList a -> a -> CatList a
```

#### `uncons`

``` purescript
uncons :: forall a. CatList a -> Maybe (Tuple a (CatList a))
```


