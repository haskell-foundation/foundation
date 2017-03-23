## 0.0.5

* Generalize monadic map (mapM, mapM\_)
* HostName type
* Network address / name resolution

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
