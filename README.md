# odin <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build status](https://github.com/mrc-ide/odin/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/odin/actions)
[![](https://www.r-pkg.org/badges/version/odin)](https://cran.r-project.org/package=odin)
[![CodeFactor](https://www.codefactor.io/repository/github/mrc-ide/odin/badge)](https://www.codefactor.io/repository/github/mrc-ide/odin)
<!-- badges: end -->

`odin` implements a high-level language for describing and implementing ordinary differential equations in R.  It provides a "domain specific language" (DSL) which _looks_ like R but is compiled directly to C.  The actual solution of the differential equations is done with the [`deSolve`](https://cran.r-project.org/package=deSolve) package, giving access to the excellent Livermore solvers (`lsoda`, `lsode`, etc), or with [`dde`](https://cran.r-project.org/package=dde) for use with delay differential equations.

* The DSL is _declarative_ reflecting the mathematical nature of the equations (typically ordinary differential equations are simple mathematical relationships, so the order should not matter).
* It includes support for equations that involve vectors, matrices and higher dimensional arrays (up to 8!), including a high-level array indexing notation that removes the need for explicit looping.
* Delay differential equations are supported, including when the delayed quantities are arbitrarily complicated expressions of variables.
* Interpolation functions can be used to include time-varying quantities into the model (piecewise constant, linear and spline interpolation is supported, using [`cinterpolate`](https://cran.r-project.org/package=cinterpolate).
* The equations are analysed before compilation so that parts that do not depend on time are not included in the final derivative calculations.
* Supports user-supplied parameters for any part of the system.
* Supports a large number of mathematical functions (see the [functions vignette](https://mrc-ide.github.io/odin/articles/functions.html)) for a complete list.

In addition, the same machinery can be used to generate discrete-time models that proceed over a set of steps (rather than through continuous time).  These may be stochastic and make use of any of R's random number functions.

`odin` works using code generation; the nice thing about this approach is that it never gets bored.  So if the generated code has lots of tedious repetitive bits, they're at least likely to be correct (compared with implementing yourself).

## Background

The "deSolve" package for R is the de-facto way of solving differential equations in R; it provides excellent solvers and has remained stable for over a decade.  However, users must implement equations in R and suffer a large speed cost, or implement their equations in C which is (depending on the complexity of the system) either routine and a bit boring, or complicated and error prone.  This translation can be especially complicated with delay differential equations, or with models where the variables are more naturally stored as variable sized arrays.

Apparently not many people know that `deSolve` can use target functions written in C rather than just in R.  This is described in detail in the excellent "compiledCode" vignette (`vignette("compiledCode")` or [online](https://cran.r-project.org/package=deSolve/vignettes/compiledCode.pdf).

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

## Example

The Lorenz attractor above can be implemented as:

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

The connection to the R and C versions in the section above should be fairly clear.  The code above is never actually evaluated though; instead it is parsed and used to build up C code for the model.

Note that this includes initial conditions; all odin models include specifications for initial conditions because the ordering of the variables is arbitrary and may be re-ordered.

This generates an object that can be used to integrate the set of differential equations, by default starting at the initial conditions specified above (though custom initial conditions can be given).  The equations are translated into C, compiled, loaded, and bundled into an object.  `lorenz` here is a function that generates an instance of the model.

```r
mod <- lorenz()
t <- seq(0, 100, length.out = 50000)
y <- mod$run(t)
```

For more complicated examples, check out an [age structured SIR model](https://github.com/mrc-ide/odin/blob/master/tests/testthat/examples/array_odin.R), and for more details see the [main package vignette](https://mrc-ide.github.io/odin/articles/odin.html)

## Limitations

Writing this has given me a much greater appreciation of the difficulties of writing compiler error messages.

This does not attempt to _generally_ translate R into C (though very simple expressions are handled) but only a small subset that follows the stereotyped way that R+C ODE models tend to be written.  It tries to do things like minimise the number of memory allocations while preventing leaks.  The generated code is designed to be straightforward to read, leaving any really funky optimisation to the compiler.

Because this relies on code generation, and the approach is partly textual, some oddities will appear in the generated code (things like `n + 0`).  Over time I'll remove the most egregious of these.  It's probable that there will be some unused variables, and unused elements in the parameters struct.

## Prior work

ODEs seem particularly suitable for code generation, perhaps because of the relative simplicity of the code.  As such, there is a lot of prior work in this area.  Many of these tools are heavily tailored to suit a particular domain.

In R:

* [`RxODE`](https://cran.r-project.org/package=RxODE) - focussed on pharmacokinetic models, but suitable in the same domain as many odin models.  Does not include support for delay equations, automatic arrays or discrete/stochastic systems and uses it's own solvers rather than interfacing with existing ones.  Notably it also uses R as the host language for the DSL rather than requiring the user to write code in strings or in a custom language.
* [`rodeo`](https://cran.r-project.org/package=rodeo) focussed on biochemical reactions based around the [Petersen matrix](https://en.wikipedia.org/wiki/Petersen_matrix).  Creates code for use with [`deSolve`](https://cran.r-project.org/package=deSolve)
* [`cOde`](https://cran.r-project.org/package=deSolve) creates code for use with [`deSolve`](https://cran.r-project.org/package=deSolve) and [`bvpSolve`](https://cran.r-project.org/package=deSolve).  Models are entered as vector of strings which resembles C or R code.  Automatic generation of Jacobian matrices is supported.
* [`mrgsolve`](https://cran.r-project.org/package=mrgsolve) is focussed on models in quantitative pharmacology and systems biology.  It bundles its own solvers, and uses it's own `PKMODEL` language ([example](https://github.com/metrumresearchgroup/mrgsolve/blob/master/inst/models/pk1.cpp)).

In other languages:

* [`Paraiso`](https://hackage.haskell.org/package/Paraiso) in Haskell
* [`VFGEN`](https://github.com/WarrenWeckesser/vfgen) in C++
* [`pygom`](https://github.com/PublicHealthEngland/pygom) in Python for solving compartmental models

## Installation

Install odin from CRAN with

```
install.packages("odin")
```

Alternatively, you can install a potentially more recent version of odin from the [`mrc-ide` drat repository](https://mrc-ide.github.io/drat/)

```r
# install.packages("drat") # -- if you don't have drat installed
drat:::add("mrc-ide")
install.packages("odin")
```

You will need a compiler to install dependencies for the package, and to build any models with odin.  Windows users should install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).  See the relevant section in [R-admin](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#The-Windows-toolset) for advice.  Be sure to select the "edit PATH" checkbox during installation or the tools will not be found.

The function `odin::can_compile()` will check if it is able to compile things, but by the time you install the package that will probably have been satisfied.

The development version of the package can be installed directly from github if you prefer with:

```r
devtools::install_github("mrc-ide/odin", upgrade = FALSE)
```

## License

MIT © Imperial College of Science, Technology and Medicine
