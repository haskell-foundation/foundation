## 0.0.4

* Expose Sequential from Foundation
* Export internal withPtr for optimisation
* Export `ifThenElse`
* Use the proper `String` type for error instead of `[Char]`

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
