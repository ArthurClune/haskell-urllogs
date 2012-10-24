haskell-urllogs
===============

Code to parse Squid logfiles

$ cabal configure   
$ cabal build  
$ ./dist/build/urllogs/urllogs +RTS -A100m -RTS access.log.gz
 
For testing:  

$ cabal configure --enable-library-profiling --ghc-option=-auto-all --enable-executable-profiling --enable-tests
$ cabal build
$ cabal test
