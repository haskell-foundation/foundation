## 0.0.11

* Add Hlint support (configuration file), and default travis job
* Property report error through with the ASCII, UTF16, UTF32 string decoders
* Fix issue with OSX < Sierra
* Improve Parser and fix backtracking issue
* Strictify UArray to contains a strict ByteArray#
* Improve any & all for most collection types
* Improve minimum & maximum for most collection types
* Add foldl1 & foldr1
* Add takeWhile & dropWhile
* Remove foldl
* Add basic String builder
* Add String replace function
* Add conduit sourceList
* Improve performance of String uncons, unsnoc, filter and fromBytes UTF8 (validate)
* Improve UArray filter
* Fix compilation issue on windows with `clock_gettime` which is not available though all possible compilation mode

## 0.0.10

* Cleanup collection APIs that take a lone Int (length, take, drop, splitAt, ..) to take a CountOf
* Rename Size to CountOf
* Add basic time functions
* Add os dependent timing capability
* Add simple pattern matching for test names with checks.
* add '--list-tests' for checks
* Optimise Eq and Ord for blocks and uarray

## 0.0.9

* Introduce Block & MutableBlock which represent a chunk of memory without slices
  and are faster/leaner in many cases.
* Cleanup String code and some primitives boundaries
* Fix storable alignment tests
* Add These data type (either a, b or both)
* Implement checks command line
* Improve checks terminal output
* drop support for GHC 7.4 and GHC 7.6
* Improve performance of copy out of block and uarray

## 0.0.8

* Add MonadReader and MonadState
* Improve performance of numerical read parsers (integral, double)
* Improve precision of double read parser
* Add Textual conduit combinator (fromBytes, toBytes, lines)
* Add DList
* Fix building on latest Win32, RHEL 5.8
* Add NormalForm
* Export some functions in Internal module to manipulate unboxed mutable array

## 0.0.7

* Improve Checks: random seed, new properties and improved printing
* Add ability to parse Natural, Integer, and Double from String
* Temporarily remove compilation of experimental network resolution introduced in 0.0.5 for windows building.
* Cleanup Offset and Size fixing some bug in String module

## 0.0.6

* Fix build on Centos 6.x / older linux distribution
* Improve test checks generators

## 0.0.5

* Generalize monadic map (mapM, mapM\_)
* HostName type
* Network address / name resolution
* Fix compilation on FreeBSD & OpenBSD
* Initial re-implementation for property tests and tests orchestration
* Fix bug in splitElem, and breakElem
* Improve splitOn to return empty elements
* Fix API bug for snoc and cons in Chunked UArray
* Add UUID
* Check API
* Fix compilation on !x86

## 0.0.4

* Add Conduit for all your streaming needs
* Expose Sequential from Foundation
* Export internal withPtr for optimisation
* Export `ifThenElse`
* Use the proper `String` type for error instead of `[Char]`
* Add `any` and `all` to `Collection`
* Add defaulting to Integer and Double for numerical types
* Add negation for Double and Float (and their associated C types)
* Add/Export system bindings (Posix file/memory handling, Linux Inotify)
* Add Big Endian (BE) / Little Endian (LE) wrapping types
* Add a way to transform an UArray into Hexadecimal/Base16
* Add IPv4 and IPv6 types

## 0.0.3

Monad:

* Add MonadCatch and MonadThrow classes
* Add Transformer base class (MonadTrans)
* Add IdentityT, StateT, ReaderT

Build:

* Fix build on !x86

## 0.0.2

Classes:

* Add `Bifunctor`
* Implement Better storable type class (#111)
* Expose Nthable for GHC >= 7.10 (product type getter)
* Split basic function from `Sequential` to `Collection`
* show return a Foundation `String` now instead of `[Char]`

Numerical:
* Overhaul of numerical classes (`Integral`, `Rational`, `Divisible`, ..)
* add IntegralRounding (i.e. rounding from floating types)
* Expose IEEE manipulation stuff
* Expose all trigonometry functions in `Foundation.Math.Trigonometry`
* Export `Natural` (Unsigned `Integer`)

Collection:
* Add partition
* Add isPrefixOf and isSuffixOf
* Add ArrayBuilder machinery
* Add `String` parser
* Add minimum and maximum to Collection.
* Export Foldable and Collection in Foundation
* add head,last,tail,init

Types:
* Basic `ArrayUArray` support (Array of unboxed Array)
* Add instance for `Float` and `Double` for numerical
* Boxed array: add native slicing in the type
* add `NonEmpty` type
* Add some Data declaration for based type

Hashing:
* Hashing: add FNV, SipHash hash functions family
* Hashable: add support to hash types

Random support:
* Add support for system entropy
* Add pseudo random generation capability using a ChaCha core.

## 0.0.1

* Initial version
