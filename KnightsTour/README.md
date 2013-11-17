Knight's Tour
=============

My first try at solving [this problem](http://en.wikipedia.org/wiki/Knight's_tour) 


Compile with

    ghc -O2 KnightsTourSpec.hs -rtsopts -threaded

Run with

    ./KnightsTourSpec +RTS -N4

You can change de 4 by the number of processes you want (min 1, but not recomended).
