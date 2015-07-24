Rawr
====

[![Build Status][3]][2]

This is an implementation of the [roaring bitmaps][1] data structure in
Haskell. Roaring bitmaps is a compressed bitmap data structure which offers
better compression and performance than other compressed bitmaps in many
situations.

[1]: http://www.roaringbitmap.org/
[2]: https://travis-ci.org/thsutton/rawr
[3]: https://travis-ci.org/thsutton/rawr.svg?branch=master

Testing
-------

`rawr` comes with a small and incomplete suite of property tests. You
can run them using `stack test` or, if you use cabal-install,

````
cabal configure --enable-tests
cabal test
````

`rawr` is tested using the `2.14` and `2.18` LTS Stackage snapshots
using the `stack-2.14.yaml` and `stack-2.18.yaml` files included in
the repository.
