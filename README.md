haskell-urllogs
===============

Code to parse Squid logfiles
```
$ cabal configure --enable-tests
$ cabal build
$ cabal test
$ ./dist/build/urllogs/urllogs +RTS -A100m -RTS access.log.gz
 ```
See src/urllogs.hs for an example of using the library with conduits
