# 1.1.1.2

* Support GHC 9.8
* Fix broken test

# 1.1.1.1

* Support GHC 9.6

# 1.1.1.0

* Expose `lookupTemplate` and `splitTemplatePath`

* Bump dependency bounds for 9.4

# 1.1

* Remove pandoc and pandocBS

* Stop exporting readProcessWithExitCode'

* Remove -S and --no-wrap arguments to pandoc for compatibility with both 1.x
  and 2.x versions of the pandoc command line tool

* Bump map-syntax lower bound to fix 8.4 build problem

# 1.0.1.3

* Add Semigroup instances to support GHC 8.4

# 1.0.1.0

* Change benchmark from an executable section to a benchmark section in the
  cabal file.  This eliminates the criterion dependency when doing "cabal
  install heist".
* Export manyWith

# 1.0.0.1

* Drop the dependency on `errors` packages from heist testsuite and benchmark
* Fix nested splice namespace warning bug (issue #85)

# 1.0.0.0

* Switch from MonadCatchIO-transformers to monad-control for Snap 1.0

# 0.14.0

See http://snapframework.com/blog/2014/09/24/heist-0.14-released

* Add namespace support (hcNamespace and hcErrorNotBound)
* Add tellSpliceError for generalized error reporting
* Restructured HeistConfig, export lenses instead of field accessors
* Moved old HeistConfig into SpliceConfig
* Factored SpliceAPI module out into separate map-syntax package

# 0.13.0

See http://snapframework.com/blog/2013/09/09/snap-0.13-released

* Simpler compiled splice API
* New splice syntax

