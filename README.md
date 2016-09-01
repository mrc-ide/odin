# odin

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/richfitz/odin.svg?branch=master)](https://travis-ci.org/richfitz/odin)
[AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/richfitz/odin?branch=master&svg=true)](https://ci.appveyor.com/project/richfitz/odin)
[![codecov.io](https://codecov.io/github/richfitz/odin/coverage.svg?branch=master)](https://codecov.io/github/richfitz/odin?branch=master)

![](https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Odin_%28Manual_of_Mythology%29.jpg/250px-Odin_%28Manual_of_Mythology%29.jpg)

`odin` implements a high-level language for describing and implementing ordinary differential equations in R.  It provides a "domain specific language" (DSL) which _looks_ like R but is compiled directly to C.  The actual solution of the differential equations is done with the deSolve package, giving access to the excellent Livermore solvers (`lsoda`, `lsode`, etc).

**Warning: This project is in the early scoping stages; do not use for anything other than amusement/frustration purposes**

# Background

The "deSolve" package for R is the de-facto way of solving differential equations in R; it provides excellent solvers and has remained stable for over a decade.  However, users must implement equations in R and suffer a large speed cost, or implement their equations in C which is (depending on the complexity of the system) either routine and a bit boring, or complicated and error prone.  This translation can be especially complicated with delay differential equations, or with models where the variables are more naturally stored as variable sized arrays.

# Overview

This package aims to provide a high level domain specific language (DSL) that allows specifying ODEs with the simplicity of R, but immediately compiling out to C, so that the system runs at native speed.

* The DSL is _declarative_ reflecting the mathematical nature of the equations (typically ODEs are simple mathematical relationships, so the order should not matter).
* It includes support for equations that involve vectors, matrices and 3d arrays, including a high-level array indexing notation that removes the need for looping.
* Delay equations are supported, including when the delayed quantities are expressions of variables.
* The equations are analysed before compilation so that parts that do not depend on time are not included in the final derivative calculations.
* Supports user-supplied parameters for any part of the system.


`odin` works using code generation; the nice thing about this approach is that it never gets bored.  So if the generated code has lots of boring repetitive bits, they're at least likely to be correct (compared with implementing yourself).

For example (and because *all* ODE software seems to like using it), here is the standard Lorenz attractor model in `odin`:

```r
lorenz <- odin::odin({
  ## Derivatives
  deriv(y1) <- sigma * (y2 - y1)
  deriv(y2) <- R * y1 - y2 - y1 * y3
  deriv(y3) <- -b * y3 + y1 * y2

  ## Initial conditions
  initial(y1) <- 10.0
  initial(y2) <- 1.0
  initial(y3) <- 1.0

  ## parameters
  sigma <- 10.0
  R     <- 28.0
  b     <-  8.0 / 3.0
})
```

This generates an object that can be used to integrate the set of differential equations, by default starting at the initial conditions specified above (though custom initial conditions can be given).  The equations are translated into C, compiled, loaded, and bundled into an object.

```r
mod <- lorenz()
t <- seq(0, 100, length.out=1000)
y <- mod$run(t)
```

For more complicated examples, check out an [age structured SIR model](tests/testthat/examples/array_odin.R), and for more details see the [vignette](https://richfitz.github.io/odin/vignettes/odin.html)

# Notes on development

The idea is to be able to convert a set of highly restricted R code into a set of differential equations that can be compiled into C.  This will simplify the deSolve/R/C use-case and reduce the amount of boilerplate and development time.  The generated code, while not being designed to be edited, should be fairly simple to understand.  In future versions, R interfaces might be generated to make it easy to test the model.

Berkeley Madonna is the inspiration for the DSL that we'll build up.

* declarative interface
* array equation syntax
* delay equations

# Special functions that will be allowed

* `length` and `dim`; for getting matrix dimensions of 1 and multidimensional arrays (respectively) [done]
* a reasonably large set of mathematical constructs, including all basic R operators (not all implemented yet)
* something to declare arbitrary functions available to R (for initialisation only and not for the derivatives calculations) as it's ok to wear that cost once.
* something to declare and use arbitrary pure C functions.

# Initial conditions

In contrast to deSolve, models produced by `odin` will compute their own initial conditions.  This is because it can be convenient to specify initial conditions in terms of parameters shared with the dynamic parts of the model.  Support for user-specified (i.e. non-compiled) parameters is available via the function `user`, which currently only accepts scalars.

# User parameters

Typically, ODEs form a part of a larger bit of machinery (e.g., inference in a ML or Bayesian framework) where they will be repeatedly evaluated at a series of parameters.  `odin` allows for "user" parameters, which may include defaults.  Some support is provided for accepting arrays (1, 2 and 3 dimensional) as user parameters too.

# Generating package code

Currently the approach is focussed on generating code at to be used within one session, but soon the package will have helpers to generate code for a package.

# Limitations

Writing this has given me a much greater appreciation of the difficulties of writing compiler error messages.

This does not attempt to translate R into C (though very simple expressions are handled) but only a small subset that follows the sterotyped way that R+C ODE models tend to be written.  It tries to do things like minimise the number of memory allocations while preventing leaks.

The code generated by `odin` may not make sense, may not compile and may crash R.  Over time I will try to catch more of these during the parse step, but there's a lot of checking to do.  For now, be warned.

Because this is very new, it is quite possible that the code that is generated will not correspond to your model.  You are advised to read the generated code and to test your model thoroughly before assuming it is doing the right thing.  The generated code is designed to be straightforward to read, leaving any really funky optimisation to the compiler.

Because this relies on code generation, and the approach is partly textual, some oddities will appear in the generated code (things like `n + 0`).  Over time I'll remove the most egregious of these.  It's probable that there will be some unused variables, and unused elements in the parameters struct.

# Background

Apparently not many people know that `deSolve` can use target functions written in C rather than just in R.  This is described in detail in the excellent "compiledCode" vignette (`vignette("compiledCode")` or [online](https://cran.r-project.org/web/packages/deSolve/vignettes/compiledCode.pdf).

While the `deSolve` authors are bearish on the benefits of this, I have often seen performance improvements of over 100x.  Where an ODE is being used in application where it is called repeatedly (e.g., an optimisation or MCMC) the cost of rewriting the system pays itself back.

For simple systems the rewriting is essentially mechanical.  The lorenz attractor could be implemented in R as:

```r
lorenz <- function(t, y, parms) {
  sigma <- parms[1]
  R <- parms[2]
  b <- parms[3]
  y1 <- y[1]
  y2 <- y[2]
  y3 <- y[3]
  list(c(sigma * (y2 - y1),
         R * y1 - y2 - y1 * y3,
         -b * y3 + y1 * y2))
}
```

and in C as

```c
void initmod(void (* odeparms)(int *, double *)) {
  int N=3;
  odeparms(&N, parms);
}
void lorenz(int *n, double *t, double *y, double *dydt, double *yout, int *ip) {
  double sigma = parms[0];
  double R = parms[1];
  double b = parms[2];
  double y1 = y[0];
  double y2 = y[2];
  double y3 = y[3];
  dydt[0] = sigma * (y2 - y1);
  dydt[1] = R * y1 - y2 - y1 * y3;
  dydt[2] = -b * y3 + y1 * y2;
}
```

The connection between the two languages should be fairly obvious.  As systems get more complicated much of the difficulty of writing the systems in C becomes the tedium of book keeping as parameters and state vectors are unpacked, rather than any deep programming challenges.  Modifying large systems is a particular challenge as technical debt can accrue quickly.

The core job of `odin` is to simplify this transition so that models can be both developed and solved rapidly.

# Installation

```r
devtools::install_github("richfitz/odin")
```

You will need a working compiler.  `odin::can_compile()` will check if it is able to compile things.
