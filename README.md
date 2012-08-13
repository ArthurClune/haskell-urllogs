haskell-urllogs
===============

Sample code written for learning Haskell

$ cabal configure   
$ cabal build  
$ ./dist/build/urllogs/urllogs access.log.gz +RTS -A100m  
 
For testing:  

cabal configure --enable-library-profiling --ghc-option=-auto-all --enable-executable-profiling    
cabal build

For production:

cabal configure --disable-library-profiling --disable-executable-profiling  
cabal build

To run:

./dist/build/urllogs/urllogs +RTS -A100M -RTS access.log.gz


