Introduction
============

Foundation is an haskell project to implement a modern & performant standard
library with various centrally maintained functionalities.

Haskell's base has been designed a while ago and shows its age. For better or
worse, it's also really hard to change base, leading to many interesting but
ultimately fruitless discussions, lots of wasted efforts, and/or
duplicated pieces and libraries.

Also many core libraries, which brings lots of welcome modern additions to
the language (text, bytestring, vector, & many others), are maintained on the
side, without coordination. For example bytestring and vector doesn't share
any code.

Foundation is trying to provide a solution to those technical and maintainance
limitations. The core ideas are:

* Provide all the interesting modern core primitives (packed UTF8 strings, arrays, others)
  and have them works well together.
* Improve types where possible; we don't want to be stuck on broken concepts because of compatibility
* Improve classes where possible, for the same reasons.
* Provide core types (e.g. uuid) by default with all the right instances
  and convertion functions, integrating well with the rest
* Improve & modernize management using modern services by default, thus we
  we use all the good services of:
  * github: for code, issues and discussions
  * cloud services for testing: travis, appveyor
  * documentation services: mkdocs

