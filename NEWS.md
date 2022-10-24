# odin 1.4.0

* Overhaul of the odin to JavaScript compilation, generates code compatible with wodin (https://reside-ic.github.io/wodin-news)

# odin 1.3.3

* More gracefully cope with filenames containing parentheses

# odin 1.3.0

* Added JavaScript support, importing code previously in [`odin.js`](https://github.com/mrc-ide/odin.js/). Models with delays are not supported (mrc-1624)

# odin 1.2.2

* Optimise some `if/else` statements where the condition can be worked out at compile time; this means only one clause may be present in the generated code (mrc-2529)

# odin 1.1.14

* Deprecate the function-style constructors (e.g., `generator(...)`) in favour of R6-style constructors (`generator$new(...)`) (#206)

# odin 1.1.13

* Allow spaces in filenames passed to `odin::odin()` (#225)

# odin 1.1.12

* New option `rewrite_constants` (via `odin::odin_options`) which attempts to rewrite all constants in the model code before generation. This can considerably reduce the number of variable lookups (mrc-2252)

# odin 1.1.11

* New option `substitutions` (via `odin::odin_options`) which can substitute in scalar `user` values at compile time (#220)

# odin 1.1.9

* New option `rewrite_dims` (via `odin::odin_options`) which will attempt to simplify common dimensions. This can reduce the number of variables carried around in the model as these are typically very redundant and also known at compile time (mrc-2093)

# odin 1.1.8

* Annotate equations with `# ignore.unused` to locally suppress messages about unused variables (mrc-2122)

# odin 1.1.6

* odin no longer warns about use of index variables on rhs as new behaviour has been established for a while (#136)

# odin 1.1.4

* Better error messages when C identifiers (e.g., `int`) are used in equations (reported by @rhysinward, @katietiley and @cwhittaker1000)

# odin 1.1.2

* Support for `config(include)` has been expanded, working for R models and designed to be extensible for other targets (mrc-2016)

# odin 1.1.0

* The basic infrastructure has been overhauled, which will make some alternative compilation targets easier to support. We now use `pkgbuild` for the compilation which should ease debugging, and odin code compiled into packages will no longer issue a slew of warnings (and cooperate with automatic routine registration). This refactor has caused a few minor breaking changes:
  - `$initial()` always requires time, even if it is ignored
  - `$set_user()` and construction no longer work with positional argument matching - all arguments must be named
  - The `$ir` field has become a method; add parentheses after it
  - The `compiler_warnings` option has been removed

# odin 1.0.7

* Support for `as.integer()` to cast to integer for use within array expressions (#200)

# odin 1.0.6

* Fixed bug in code generation for initial conditions involving sums of matrices (#196)

# odin 1.0.3

* Support for multivariate hypergeometric function via the odin function `rmhyper()` - there is no analogue for this in base R. Like `rmultinom` this returns a vector and the interface is subject to possible change (`mrc-1621`)
* New vignette `functions`, which briefly describes all supported odin functions (`mrc-1623`)

# odin 1.0.2

* Support for 2-argument round (e.g., `round(1.23, 1)` is 1.2), and enforce the same 0.5 rounding behaviour as R when used from C (`mrc-511`, #116, #179)

# odin 1.0.0

* Initial release to CRAN

# odin 0.2.6

* Default history size is back to 10,000 steps (not 1,000) as in pre-0.2.0 (`mrc-327`, #176)

# odin 0.2.4

* `odin_package` now works with R staged installation as introduced in 3.6.0 ([#170](https://github.com/mrc-ide/odin/pull/170), `mrc-257`)

# odin 0.2.3

* Better error messages for some invalid uses of array indices that previously manifested as compiler errors [#166](https://github.com/mrc-ide/odin/issues/166)

# odin 0.2.2

* Fix two memory leaks ([#163](https://github.com/mrc-ide/odin/issues/163)); these leaked once per model creation.

# odin 0.2.1

* Continuous time models with delays can now return derivatives ([#161](https://github.com/mrc-ide/odin/issues/161), [#162](https://github.com/mrc-ide/odin/issues/162))

# odin 0.2.0

A complete rewrite of the odin engine, designed to increase future maintainability but have few user-visible effects.  In brief, this does add

* Ability to transpile to R (removing the need for a C compiler, though creating code that is necessarily much slower than the compiled version).  Pass `target = "r"` to use this.
* A new intermediate representation for odin models that can be used to determine features of a model
* Better caching ([#64](https://github.com/mrc-ide/odin/issues/64))
* Delays on discrete time models are much more efficient and work properly with stochastic equations ([#72](https://github.com/mrc-ide/odin/issues/72), [#98](https://github.com/mrc-ide/odin/issues/98))

This does introduce a few user-visible **breaking changes**:
  - use of array indices outside of an array subset (e.g., `x[] <- i`) produces output that is off-by-one compared with the previous version (see [#136](https://github.com/mrc-ide/odin/issues/136))
  - The "safe" mode has been removed, at least for now.  This failed to compile for complex models and was not well used.  A better static check mechanism will be introduced ([#148](https://github.com/mrc-ide/odin/issues/148))
  - Arguments to `odin()` have been modified slightly
  - `odin_package()` no longer supports multi-file mode (I don't think this was ever used).
  - Argument order in generated functions, as well as variable/output order, is likely to change but this should not generally be relied upon.
  - The methods for a model object have been extensively rationalised.  I don't think that anything has been removed that anyone is using in their code.
  - The parameters passed as a list `user` are validated and providing additional parameters is a warning (by default; this can be configured to be more or less strict as desired).

Fixes many outstanding issues:
[#139](https://github.com/mrc-ide/odin/issues/139),
[#136](https://github.com/mrc-ide/odin/issues/136),
[#132](https://github.com/mrc-ide/odin/issues/132),
[#130](https://github.com/mrc-ide/odin/issues/130),
[#129](https://github.com/mrc-ide/odin/issues/129),
[#127](https://github.com/mrc-ide/odin/issues/127),
[#123](https://github.com/mrc-ide/odin/issues/123),
[#122](https://github.com/mrc-ide/odin/issues/122),
[#121](https://github.com/mrc-ide/odin/issues/121),
[#120](https://github.com/mrc-ide/odin/issues/120),
[#117](https://github.com/mrc-ide/odin/issues/117),
[#115](https://github.com/mrc-ide/odin/issues/115),
[#112](https://github.com/mrc-ide/odin/issues/112),
[#106](https://github.com/mrc-ide/odin/issues/106),
[#99](https://github.com/mrc-ide/odin/issues/99),
[#98](https://github.com/mrc-ide/odin/issues/98),
[#84](https://github.com/mrc-ide/odin/issues/84),
[#72](https://github.com/mrc-ide/odin/issues/72),
[#64](https://github.com/mrc-ide/odin/issues/64),
[#57](https://github.com/mrc-ide/odin/issues/57),
[#52](https://github.com/mrc-ide/odin/issues/52),
[#12](https://github.com/mrc-ide/odin/issues/12),
[#4](https://github.com/mrc-ide/odin/issues/4)

# odin 0.1.2

* Validate parameters in input

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
