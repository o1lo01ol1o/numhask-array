numhask-array
===

[![Build Status](https://travis-ci.org/tonyday567/numhask-array.png)](https://travis-ci.org/tonyday567/numhask-array)

Since this [post](https://tonyday567.github.io/naperian/index.html) there's been a few lessons learnt:

- Don't call n-dimensional arrays tensors.  Tensors have variance structure that is absent in working out general n-dimensional array algebra. Hence the name change.

- Type-level coding in haskell is just a little too hard (for me) when it comes to keeping track of array dimensions.


To recap, let's say we have a type-checked, n-dimensional array

```
-- >>> let a = [1..24] :: Array '[2,3,4] Int
-- >>> a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]
```

And we'd like a function that takes a list of index lists (third dimension, column and row) and extracts the "slice" from an array.

There are three main version of an array slicer contained in this repo:

unsafeSlice in NumHask.Array
---

```
> let a1 = unsafeSlice [[0,1],[2],[1,2]] a
> :t a1
a1 :: Array r0 Int
> a1 :: Array '[2,1,2] Int
[[[10, 11]],
 [[22, 23]]]
```

The Array shape has to be supplied to the type.  Not very useful.

slice in NumHask.Array
---

```
> let a2 = slice (Proxy :: Proxy ['[0,1],'[2],'[1,2]]) a
> :t a2
a2 :: Array (2 :$$$ '[1, 2]) Int
> a2
[[[10, 11]],
 [[22, 23]]]
```

The compiler worked it out, and just got a little stutter in trying to cons the new shape together. Progress, but note that the slice list had to be supplied at the type-level - no typing it in from the console.

slice in NumHask.Array.Slicer
---

```
> let a2 = slice (Proxy :: Proxy ['[0,1],'[2],'[1,2]]) a
> :t a2
a2 :: Array (2 :$$$ '[1, 2]) Int
> a2
[[[10, 11]],
 [[22, 23]]]
```

The compiler worked it out, and just got a little stutter in trying to cons the new shape together. Progress, but note that the slice list had to be supplied at the type-level - no typing it in from the console.

slicer in NumHask.Array.Slicer
---

Thanks to [Justin](https://blog.jle.im/entry/practical-dependent-types-in-haskell-2.html) for getting this together.

```
> let a = Array [1..24] :: Array '[2,3,4] Int
> let i = [[0,1],[2],[0,1]]
> slicer i a (\x -> Right (show x::Text))
Right "[[[9, 10]],\n [[21, 22]]]"
```

And there it is.  Despite types being utterly erased at run-time, slicer first deeply embeds an idea, and then enters ghc at the exact moment of erasure, does a lot of running about, continuation passing style, then emerges with the goods. Most intruigingly, the array is correctly typed but neither the user nor ghc can ever known what that type ever was.
