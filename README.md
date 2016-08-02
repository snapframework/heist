# Heist

[![Build Status](https://travis-ci.org/snapframework/heist.svg?branch=master)](https://travis-ci.org/snapframework/heist)

Heist, part of the [Snap Framework](http://www.snapframework.com/), is a
Haskell library for xml/html templating. It uses simple XML tags to bind
values to your templates in a straightforward way. For example, if you were to
put the following in a template:

    <bind tag="message">some text</bind>
    <p><message/></p>

the resulting xhtml would be

    <p>some text</p>

Likewise, if you need to add text to an attribute,

    <bind tag="special">special-id</bind>
    <div id="${special}">very special</div>

gives you

    <div id="special-id">very special</div>

Values can also be pulled from "Splices" (see 
[the documentation](http://snapframework.com/docs/tutorials/heist#heist-programming) 
for more information.)

## Building heist

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
