Block
-----

This represent a simple block of memory that is natively handled by the GC;
this is very similar to a Short ByteString, or an unboxed vector without
slicing capabilities.

Boxed & Unboxed Arrays
----------------------

From a simplistic point of view we provide the same functionalities as the
vector package, but we unify the foreign ptr arrays and the native arrays behind
one type.

Strings
-------

We don't define String to a be list of Char. Just like in many other languages
our strings are packed array of byte using the [UTF8](https://en.wikipedia.org/wiki/UTF-8) encoding.

It does re-use all the array code, and it's literally an array of bytes with
the property of containing validated UTF8-encoded unicode characters.
