How to contribute
=================

Any contributions is welcome, but a short list includes:

* Improve the code base
* Report an issue
* Fix an issue
* Improve the documentation
* Make tutorial on how to use foundation
* Make your project use foundation instead of base, report the missing coverage (IO, types, etc.), or what functionality is missing to make a succesful transition

HLint
-----

[HLint]()
is a tool for suggesting possible improvements to Haskell code.
Foundation uses HLint to maintain a high level standard of code.

It is recommended that the latest possible HLint version is used.
To install the latest version, find out what the 
[latest version of HLint in hackage](https://hackage.haskell.org/package/hlint)
is, and install it via:

```
$ stack install hlint-<VERSION>
```

To succesfully run HLint locally, you need to pass in the directory on which to run.
Foundation provides a default HLint configuration file with certain assumed exclusions, so HLint should be run from the Foundation repository root directory for the correct report to be generated.
Also, it might be necessary to pass in specific CPP flags:

```
$ hlint \
--cpp-define=__GLASGOW_HASKELL__=800 \
--cpp-define=x86_64_HOST_ARCH=1 \
--cpp-define=mingw32_HOST_OS=1 \
--report \
.
```
