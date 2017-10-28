Foundation
==========

[![Linux Build Status](https://img.shields.io/travis/haskell-foundation/foundation/master.svg?label=Linux%20build)](https://travis-ci.org/haskell-foundation/foundation)
[![Windows Build Status](https://img.shields.io/appveyor/ci/vincenthz/foundation/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/vincenthz/foundation/branch/master)
[![Doc](https://readthedocs.org/projects/haskell-foundation/badge/?version=latest)](http://haskell-foundation.readthedocs.io/en/latest/)
[![Stackage version](https://www.stackage.org/package/foundation/badge/lts?label=Stackage)](https://www.stackage.org/package/foundation)
[![Hackage version](https://img.shields.io/hackage/v/foundation.svg?label=Hackage)](https://hackage.haskell.org/package/foundation)
[![BSD3](https://img.shields.io/badge/License-BSD-blue.svg)](https://en.wikipedia.org/wiki/BSD_License)
[![Haskell](https://img.shields.io/badge/Language-Haskell-yellowgreen.svg)](https://www.haskell.org)
[![Coverage Status](https://coveralls.io/repos/github/haskell-foundation/foundation/badge.svg?branch=master)](https://coveralls.io/github/haskell-foundation/foundation?branch=master)

Documentation:

* [Read the doc](http://haskell-foundation.readthedocs.io/en/latest/)
* [Foundation on stackage](https://www.stackage.org/package/foundation)
* [Foundation on hackage](https://hackage.haskell.org/package/foundation)

Discuss:

* [FP Chat](https://fpchat-invite.herokuapp.com) `#haskell-foundation` channel
* [Gitter](https://gitter.im/haskell-foundation/foundation)

Goals
=====

* provide a base like set of modules that provide a consistent set of features and bugfixes across multiple versions of GHC (unlike base).
* provide a better and more efficient prelude than base's prelude.
* be self-sufficient: no external dependencies apart from base (or ghc packages).
* provide better data-types: packed unicode string by default, arrays.
* Better numerical classes that better represent mathematical things (No more all-in-one Num).

Usage
=====

How to use with the normal Prelude
----------------------------------

Add the `foundation` package in your cabal file, and it's recommended to import Foundation qualified if
you're planning to use with the normal Prelude:

For example:

```haskell
import qualified Foundation as F
```

It's also recommended if you're going to deal with packages using text, bytestring, vector.. to use
the `foundation-edge` package.

How to use fully without Prelude
--------------------------------

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
import Foundation
```

Advanced settings
----------------------

Please check out the chapter [Advanced Usage Options](docs/advanced-runtime.md) in the
documentation.


How to contribute
=================

Contribution guide can be found
[here](http://haskell-foundation.readthedocs.io/en/latest/contributing/).


Profiling
---------

If you want to see the core (simpl step) or the assembly (asm step), then you can build with

    stack build --ghc-options -ddump-simpl --ghc-options -ddump-asm

Note that it doesn't actually will create the necessary extra files if the modules doesn't need building.

you can then find your build by-products in:

    .stack-work/dist/<architecture>/Cabal-<CabalVersion>/build/

Each modules that get compiled will create an equivalent file in the build directory:

* ModuleName.dump-simpl
* ModuleName.dump-asm

For profiling individual programs, the following command is useful:

    stack ghc -- -O --make X.hs -prof -auto-all -caf-all -fforce-recomp

Benchmarking
------------

To get the list of benchmark:

    stack bench --benchmark-arguments -l

To compare against other libraries, you need to set the `bench-all` flag

    stack bench --flag foundation:bench-all --benchmark-arguments -l

To run a specific or set of benchmarks :

    stack bench --flag foundation:bench-all --benchmark-arguments 'types/String/SplitAt/mascii-10/Text'
    stack bench --flag foundation:bench-all --benchmark-arguments '-m prefix types/String/SplitAt'
    stack bench --flag foundation:bench-all --benchmark-arguments '-m glob types/String/SplitAt'

To register a set of benchmarks:

    stack bench --flag foundation:bench-all --benchmark-arguments "--csv $(git describe).csv"


Design
======

Foundation started on the simple idea of trying to put everything I need in one
simple and consistent package. The amazing haskell ecosystem is extremely
fragmented and maintained by different people with different goals, free time,
and style. The overall scare of not trying to change anything relatively
central (`base`, `bytestring`, `text`, `vector`, ...) for a promise of stability
has pushed many people to work on their own thing, leading to unnecessary work
duplication and further fragmentation.


Foundation uses and abuses type families.


Code Organisation
=================

Every foundation modules start by `Foundation`.

* `Foundation` is the prelude replacement module.
* `Basement.Compat` contains only compatibilty and re-export from ghc/ghc-prim/base.
* `Foundation.Primitive` is where all the lowlevel magic happens:
  * Important types that underpins many others part
  * Pervasive features
