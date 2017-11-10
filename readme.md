numhask-array
===

[![Build Status](https://travis-ci.org/tonyday567/numhask-array.png)](https://travis-ci.org/tonyday567/numhask-array) [![Hackage](https://img.shields.io/hackage/v/numhask-array.svg)](https://hackage.haskell.org/package/numhask-array) [![lts](https://www.stackage.org/package/numhask-array/badge/lts)](http://stackage.org/lts/package/numhask-array) [![nightly](https://www.stackage.org/package/numhask-array/badge/nightly)](http://stackage.org/nightly/package/numhask-array) 

An experimental array with:

- shape specified at the type level in n-dimensions
- a [vector](https://www.stackage.org/package/vector) backend
- Representable instances
- [numhask](https://www.stackage.org/package/numhask) heirarchy instances

See [Examples](src/NumHask/Array/Example.hs) for the emergent API.

Slicer
===

Since this [post](https://tonyday567.github.io/naperian/index.html) on naperian functors there's been a few lessons learnt on working with type-checked n-dimensional arrays:

- Don't call them tensors.  Tensors have variance structure that is absent in working out general n-dimensional array algebra.

- Type-level coding in haskell is tough when it comes to keeping track of array dimensions.


To recap, let's say we have a type-checked, n-dimensional array

```haskell
>>> let a = [1..24] :: Array '[2,3,4] Int
>>> a
[[[1, 2, 3, 4],
  [5, 6, 7, 8],
  [9, 10, 11, 12]],
 [[13, 14, 15, 16],
  [17, 18, 19, 20],
  [21, 22, 23, 24]]]
```

And we'd like a function that takes a list of index lists (third dimension, column and row) and extracts the "slice" from an array.

There are three main version of an array slicer contained in this repo:

[unsafeSlice](https://github.com/tonyday567/numhask-array/blob/master/src/NumHask/Array.hs#L271)
---

```haskell
> let a1 = unsafeSlice [[0,1],[2],[1,2]] a
> :t a1
a1 :: Array r0 Int
> a1 :: Array '[2,1,2] Int
[[[10, 11]],
 [[22, 23]]]
```

The Array shape has to be supplied to the type.  Not very useful.

[slice](https://github.com/tonyday567/numhask-array/blob/master/src/NumHask/Array.hs#L292)
---

```haskell
> let a2 = slice (Proxy :: Proxy ['[0,1],'[2],'[1,2]]) a
> :t a2
a2 :: Array (2 :$$$ '[1, 2]) Int
> a2
[[[10, 11]],
 [[22, 23]]]
```

The compiler worked it out, and just got a little stutter in trying to cons the new shape together. Progress, but note that the slice list had to be supplied at the type-level - no typing it in from the console.

[slicer](https://github.com/tonyday567/numhask-array/blob/master/src/NumHask/Array/Slicer.hs#L88)
---

Thanks to [Justin Le](https://blog.jle.im/entry/practical-dependent-types-in-haskell-2.html) for getting this together.

```haskell
> let a = Array [1..24] :: Array '[2,3,4] Int
> let i = [[0,1],[2],[0,1]]
> slicer i a (\x -> Right (show x::Text))
Right "[[[9, 10]],\n [[21, 22]]]"
```

Magic has happened.  Despite types being utterly erased at run-time, slicer sneakily embeds unsafeSlice deep into GHC's unconcious, then enters ghc at the exact moment of erasure, does a lot of continuation passing style running about, and then emerges with the goods before run-time sees what's going on. Most intruigingly, the array is correctly typed but neither the user nor ghc can ever known what that type was.


Accelerate
---

PKG_CONFIG_PATH=/usr/local/opt/libFFI/lib/pkgconfig stack build --ghc-options -freverse-errors

