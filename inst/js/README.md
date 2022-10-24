# odin JavaScript support

To run models in JavaScript we use support from two JavaScript packages:

* [`odin.js`](https://mrc-ide.github.io/odin-js) for continuous time (ODE) models
* [`dust.js`](https://mrc-ide.github.io/dust-js) for discrete time (and usually stochastic) models

Run `./scripts/update_js` to update these packages automatically to the most recent version (currently this uses the most recent version merged to main, but later we may switch to the most recent version published to npm once we settle on a workflow there).

The other js code here is handwritten and only used in tests:

* `dust-rng.js` provides an interface to R's random number generator (requires running via the V8 package with a host copy of R)
* `test-continuous.js` and `test-discrete.js` provide wrappers for running the wodin interface from R's tests
