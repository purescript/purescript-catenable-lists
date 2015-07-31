## Module Data.DList

#### `DList`

``` purescript
data DList a
  = DList (List a) (List a)
```

#### `empty`

``` purescript
empty :: forall a. DList a
```

#### `null`

``` purescript
null :: forall a. DList a -> Boolean
```

#### `snoc`

``` purescript
snoc :: forall a. a -> DList a -> DList a
```

#### `uncons`

``` purescript
uncons :: forall a. DList a -> Maybe (Tuple a (DList a))
```


