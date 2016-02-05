# odin

[![Project Status: Concept - Minimal or no implementation has been done yet.](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)

**Warning: This project is in the early scoping stages; do not use for anything other than amusement/frustration purposes**

> Ode Generation and Integration

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
