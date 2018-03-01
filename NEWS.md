# odin 0.0.5

* Support for multinomial distributions, which are the first example of supported vector-returning functions [#100](https://github.com/mrc-ide/odin/issues/100)
* New vignette for discrete time models, contributed by [`@thibautjombart`](https://github.com/thibautjombart) [#102](https://github.com/mrc-ide/odin/pull/102)

# odin 0.0.4

* Generate odin models in "safe" mode, where all array access is bounds-checked.  This will run slower but make debugging crashes much simpler ([#79](https://github.com/mrc-ide/odin/issues/79), [#49](https://github.com/mrc-ide/odin/issues/49))

# odin 0.0.3

* Implement caching layer [#54](https://github.com/mrc-ide/odin/issues/54) and possibly enough for [#62](https://github.com/mrc-ide/odin/issues/62).  The interface here is subject to change (currently there is no way to interact with the cache) but should be enough for most cases.  Setting the output directory explicitly will be necessary to cache across sessions.
