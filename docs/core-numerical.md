# Core Numerical

While the Num is practical, it's also not very well defined or generic enough.
For example, it's heavily skewed towards types forming a Field, which are also
typically signed.

The design used here is to try to break single piece of functionality into
their own classe, so that you can overloaded addition operation on your types
which doesn't requires an implementation for subtraction or negation that is
fundamentally impossible to write.

We also strive to provide something that is enjoyable to use for computer programs
purpose instead of exposing an extremely precise mathematical abstraction that
is only useful for corner cases.

### Integral
### HasNegation
### IsIntegral
### IsNatural

### Additive

This is a simple type class to wrap the + operation.

It has the following member 'azero', '+' and 'scale' where:

* azero is the identity element of the group over '+'
* '+' is the group addition
* 'scale' is the repeated addition of n times.

It has the following properties:

```haskell
azero + azero == azero
azero + x     == x
x     + azero == x
scale 0 x == azero
scale 1 x == x
```

This can be effectively be seen as a Monoid where mempty is azero, and (+) is mappend.

### Subtractive

This is a class to wrap the `-` operation.

However the key difference with the `-` available in base, is that while the
operand need to be of same type, the result is not constraint to be of the same
type as the operands.

The result is an associated type called `Difference` which is determined by the
operand.

```haskell
(-) :: a -> a -> Difference a
```

This doesn't cover all the possible scenario available, but the added flexibility
allow new interesting use cases.

### Multiplicative

This is a class to wrap the multiplication `*` operation.

It has the following properties:

```haskell
midentity * midentity == midentity
midentity * x == x
x * midentity == x
x ^ 0 == midentity
x ^ 1 == x
```


