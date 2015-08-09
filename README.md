# purescript-catenable-lists

Strict catenable list implementation for PureScript.

The implementation is based on a queue data type that is backed by a
pair of lists.

See the following references for further information.
* [Simple and Efficient Purely Functional Queues and Dequeues](http://www.westpoint.edu/eecs/SiteAssets/SitePages/Faculty%20Publication%20Documents/Okasaki/jfp95queue.pdf) (Okasaki 1995)
* [Purely Functional Data Structures](http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf) (Okasaki 1996)

## Installation

```bash
bower install --save purescript-catenable-lists
```

## Documentation

* [Data.CatList](docs/Data/CatList.md)
* [Data.CatQueue](docs/Data/CatQueue.md)

## Benchmarks

![cons](benchmarks/cons.png)

![snoc](benchmarks/snoc.png)

![uncons](benchmarks/uncons.png)

![append](benchmarks/append.png)
