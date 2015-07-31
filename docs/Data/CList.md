## Module Data.CList

#### `CList`

``` purescript
data CList a
  = CNil
  | CCons a (DList (CList a))
```

#### `empty`

``` purescript
empty :: forall a. CList a
```

#### `append`

``` purescript
append :: forall a. CList a -> CList a -> CList a
```

#### `snoc`

``` purescript
snoc :: forall a. CList a -> a -> CList a
```

#### `uncons`

``` purescript
uncons :: forall a. CList a -> Maybe (Tuple a (CList a))
```


