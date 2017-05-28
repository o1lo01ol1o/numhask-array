```include
other/header.md
```

<blockquote cite>
If language was given to men to conceal their thoughts, then gesture's purpose was to disclose then. ~ John Napier
</blockquote>

repo: [naperian](https://github.com/tonyday567/naperian).

[naperian tensors](https://github.com/tonyday567/naperian)
===

Tensor
---

```include
other/Tensor.md
```

Existential Slicer
---

Justin

[justin](https://gist.github.com/mstksg/629fda869802463f9c718fddd7d5c0f1)

```include
other/Slicer.md
```

Hacker Slicer
---

Tim
[tim](https://gist.github.com/o1lo01ol1o/907b850aa9bbe6c228d2db01cd137858)

type-level slice function

\begin{code}

$(promote
      [d|
  -- tSlice :: [[a]] -> [Int]
  tSlice xs = map length xs
  |])


-- | the only way I could think of to be able to talk about x in the type signature of tslice
prox :: forall x. (SingI x) => [[Int]] -> Proxy (x::[[Nat]])
prox _ = Proxy::Proxy x

-- | take a slice of a Tensor
-- >>> :set -XFlexibleContexts
-- >>> :set -XAllowAmbiguousTypes
-- >>>  tslice [[0,1],[2],[1,2]] (prox [[0,1],[2],[1,2]]) a
-- ...
-- ... Couldn't match type ‘Data.Singletons.Prelude.List.Map
-- ... LengthSym0 x0’
-- ... with ‘Data.Singletons.Prelude.List.Map LengthSym0 x’
-- ... Expected type: Tensor (MapSym2 LengthSym0 x) Int
-- ... Actual type: Tensor (TSlice x0) Int
-- ... NB: ‘Data.Singletons.Prelude.List.Map’ is a type function, and may not be injective
-- ... The type variable ‘x0’ is ambiguous
-- ...
tslice :: forall a (r::[Nat]) (x::[Nat]). (SingI r, SingI x, SingI x) =>
    [[Int]] -> Tensor (r::[Nat]) a -> Tensor (x::[Nat]) a
tslice xs f = tabulate $ \xs' -> index f (zipWith (!!) xs xs')

sliceSing :: forall a c (r::[Nat]). (SingI r) =>
    [[Int]] -> Tensor r a -> (forall x. SingI x => Tensor (x::[Nat]) a -> c) -> c 
sliceSing xs f = withSomeSing (fmap length xs) $ \nsSing ->
                       withSingI nsSing $ \t -> tslice xs t

\end{code}


Type Family Slicer
---

Lysxia

[lysxia](https://gist.github.com/Lysxia/bf4fa5adf879de309bca780a5abceff8)


defunctionalisation for the win
---

[defunctionisation](https://typesandkinds.wordpress.com/2013/04/01/defunctionalization-for-the-win/)

[faucelme](https://www.reddit.com/r/haskell/comments/67f4mw/naperian_tensors/dgr16jy/)


***

<div class="footer">
Powered by [haskell](https://haskell-lang.org/), [stack](https://docs.haskellstack.org/en/stable/README/) and [pandoc](http://pandoc.org/).
</div>
