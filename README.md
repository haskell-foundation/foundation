foundation
=======

[![Build Status](https://travis-ci.org/vincenthz/hs-foundation.png?branch=master)](https://travis-ci.org/vincenthz/hs-foundation)
[![BSD](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)

Documentation: [foundation on hackage](http://hackage.haskell.org/package/foundation)

Goals
=====

* provide a base like sets of modules that provide a consistent set of features and bugfixes across multiple versions of GHC (unlike base).
* provide a better and more efficient prelude than base's prelude.
* be self-sufficient: no external dependencies apart from base (or ghc packages).
* provide better data-types: packed unicode string by default, arrays.
* Better numerical classes that better represent mathematical thing (No more all-in-one Num).
