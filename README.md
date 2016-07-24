Foundation
==========

[![Build Status](https://travis-ci.org/vincenthz/hs-foundation.png?branch=master)](https://travis-ci.org/vincenthz/hs-foundation)
[![BSD](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)

Documentation: [foundation on hackage](http://hackage.haskell.org/package/foundation)

Goals
=====

* provide a base like set of modules that provide a consistent set of features and bugfixes across multiple versions of GHC (unlike base).
* provide a better and more efficient prelude than base's prelude.
* be self-sufficient: no external dependencies apart from base (or ghc packages).
* provide better data-types: packed unicode string by default, arrays.
* Better numerical classes that better represent mathematical things (No more all-in-one Num).

How to use
==========

Disable the built-in prelude at the top of your file:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
```

Or directly in your project cabal file:

```haskell
Default-Extensions: NoImplicitPrelude
```

Then in your modules:

```haskell
import Core
```

How to contribute
=================

Any contributions is welcome, but a short list includes:

* Improve the code base
* Report an issue
* Fix an issue
* Improve the documentation
* Make tutorial on how to use foundation
* Make your project use foundation instead of base, report the missing coverage (IO, types, etc.), or what functionality is missing to make a succesful transition


Design
======

Foundation started on the simple idea of trying to put everything I need in one
simple and consistent package. The amazing haskell ecosystem is extremely
fragmented and maintained by different people with different goals, free time,
and style. The overall scare of not trying to change anything relatively
central (base, bytestring, text, vector ..) for a promise of stability has pushed
many people to work on their own thing, leading to unnecessary work duplication
and further fragmentation.


Foundation uses and abuses type families.
