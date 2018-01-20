numhask-array
===

[![Build Status](https://travis-ci.org/tonyday567/numhask-array.png)](https://travis-ci.org/tonyday567/numhask-array) [![Hackage](https://img.shields.io/hackage/v/numhask-array.svg)](https://hackage.haskell.org/package/numhask-array) [![lts](https://www.stackage.org/package/numhask-array/badge/lts)](http://stackage.org/lts/package/numhask-array) [![nightly](https://www.stackage.org/package/numhask-array/badge/nightly)](http://stackage.org/nightly/package/numhask-array) 

An experimental array with:

- a polymorphic container
- shape specified at the type level
- Representable instances
- [numhask](https://www.stackage.org/package/numhask) heirarchy instances

See [Examples](src/NumHask/Array/Example.hs) for the emergent API.

Workflow
---

pkg_config hack courtesy of accelerate

```
PKG_CONFIG_PATH=/usr/local/opt/libFFI/lib/pkgconfig stack build --ghc-options -freverse-errors
```

