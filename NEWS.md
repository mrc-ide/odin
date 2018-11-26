# odin 0.1.4

* DDE models can be restarted, using `dde`'s `dopri_restart` function.  This is only a useful thing for delay models as ODE models can always be trivially restarted [#141](https://github.com/mrc-ide/odin/issues/141).

# odin 0.1.3

* Graph data includes output variables [#140](https://github.com/mrc-ide/odin/issues/140)

# odin 0.1.2

* Validate parameters in input, allowing for user parameters that are discrete or fall within a range [#137](https://github.com/mrc-ide/odin/issues/137)

# odin 0.0.10

* Replication interface for discrete models (primarily for stochastic models) [#125](https://github.com/mrc-ide/odin/issues/125).

# odin 0.0.9

* Delay differential equations may use interpolated functions within their delays [#130](https://github.com/mrc-ide/odin/issues/130)

# odin 0.0.6

* Fix caching of models specified with files (rather than inline code) [#99](https://github.com/mrc-ide/odin/issues/99)

# odin 0.0.5

* Support for multinomial distributions, which are the first example of supported vector-returning functions [#100](https://github.com/mrc-ide/odin/issues/100)
* New vignette for discrete time models, contributed by [`@thibautjombart`](https://github.com/thibautjombart) [#102](https://github.com/mrc-ide/odin/pull/102)

# odin 0.0.4

* Generate odin models in "safe" mode, where all array access is bounds-checked.  This will run slower but make debugging crashes much simpler ([#79](https://github.com/mrc-ide/odin/issues/79), [#49](https://github.com/mrc-ide/odin/issues/49))

# odin 0.0.3

* Implement caching layer [#54](https://github.com/mrc-ide/odin/issues/54) and possibly enough for [#62](https://github.com/mrc-ide/odin/issues/62).  The interface here is subject to change (currently there is no way to interact with the cache) but should be enough for most cases.  Setting the output directory explicitly will be necessary to cache across sessions.
