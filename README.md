Heist 0.1.1
-----------

Heist, part of the Snap Framework (http://www.snapframework.com/), is a Haskell
library for xhtml templating. FIXME: more description here

Building heist
--------------------

The heist library is built using [Cabal](http://www.haskell.org/cabal/) and
[Hackage](http://hackage.haskell.org/packages/hackage.html). Just run

    cabal install

from the `heist` toplevel directory.


## Building the Haddock Documentation

The haddock documentation can be built using the supplied `haddock.sh` shell
script:

    ./haddock.sh

The docs get put in `dist/doc/html/`.


## Building the testsuite

To build the test suite, `cd` into the `test/` directory and run

    $ cabal configure
    $ cabal build

From here you can invoke the testsuite by running:

    $ ./runTestsAndCoverage.sh 


The testsuite generates an `hpc` test coverage report in `test/dist/hpc`.
