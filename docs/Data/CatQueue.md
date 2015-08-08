## Module Data.CatQueue

This module defines a strict queue.

#### `CatQueue`

``` purescript
data CatQueue a
  = CatQueue (List a) (List a)
```

##### Instances
``` purescript
instance showCatQueue :: (Show a) => Show (CatQueue a)
```

#### `empty`

``` purescript
empty :: forall a. CatQueue a
```

#### `null`

``` purescript
null :: forall a. CatQueue a -> Boolean
```

#### `cons`

``` purescript
cons :: forall a. a -> CatQueue a -> CatQueue a
```

#### `snoc`

``` purescript
snoc :: forall a. CatQueue a -> a -> CatQueue a
```

#### `uncons`

``` purescript
uncons :: forall a. CatQueue a -> Maybe (Tuple a (CatQueue a))
```


