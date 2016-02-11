# odin

[![Project Status: Concept - Minimal or no implementation has been done yet.](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)

**Warning: This project is in the early scoping stages; do not use for anything other than amusement/frustration purposes**

> Ode Generation and Integration

# Scope

A declarative way of running ODEs at native (C) speed in R.  Implements a domain specific language based on a subset of R to a set of differential equations suitable for solving with deSolve.

# Notes on development

The idea is to be able to convert a set of highly restricted R code into a set of differential equations that can be compiled into C and run at native speed.  This will simplify the deSolve/R/C use case and reduce the amount of boilerplate and development time.  The generated code, while not being designed to be edited, should be fairly simple to understand.  R interfaces should be generated to make it easy to test the model.

Berkeley Madonna is the inspiration for the DSL that we'll build up.

* declarative interface
* array equation syntax

# Special functions that will be allowed

* dim, nrow, ncol; getting matrix dimensions
* a large set of mathematical constructs, and all basic R operators
* something to declare arbitrary functions available to R (for initialisation only and not for the derivatives calculations).
* Some type declaration;
  `type(foo) <- bool`
  `type(bar) <- int`

# Optimisation

It might be nice to declare any variables as interactive to the compiler; these would appear in the generated interface and anything set by them would be allowed to change.

# Arrays

Array indices need to be added to the dependencies.  Dynamic vs static arrays need to be identified during processing because we might allocate the memory differently for them.

# Constants

Things that are not user configurable should hit the file as a bunch of constants (e.g. `#define`)

# Initial conditions

By default, if no initial conditions are provided, we should require a
user-provided set of equations.  Define a thing USER perhaps?

Should generate a function: `initial_conditions` which takes as input the initial time (if, and probably only if, the initial conditions are a function of time), and a set of user parameters.

These should be named:

```
sigma <- user(sigma)
```

or

```
sigma <- user(parameters[1])
```

or even

```
sigma <- user(parameters[1:na])
```

# deSolve compatibility

This package is designed to solve large sets of differential equations where it is not necessarily feasible or convenient to manually construct the state vector.  As such, an additional "initialisation" step will be needed in addition to the deSolve calls.  In a returned model, the initialisation function *must* be called before integration.

An alternative approach would be to include the initial state in the "parameters" object.  That would probably do an OK job of initialising while staying a bit closer to the deSolve interface; it also guarantees that the delay functions will always get access to the values.

# Development plan

* Scope the core features we want to support:
  - R'ish DSL
  - delay differential equations
  - generation of initial conditions from parameters
  - compilation to C code
  - array indexing for equations
* Construct a roughly working version with a bunch of test cases
  - implemted in BM, and output saved to CSV files
  - implemented naively in R for deSolve; speed is not an issue here
  - implemented with our DSL
  for each of the test cases we'd check that the generated model compiles and gives the same output.  The BM to R checks will be done but really simple.
