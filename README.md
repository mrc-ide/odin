# odin

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/mrc-ide/odin.svg?branch=master)](https://travis-ci.org/mrc-ide/odin)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/wmdbqbgrqw26xan5/branch/master?svg=true)](https://ci.appveyor.com/project/richfitz/odin-hpgj3/branch/master)
[![codecov.io](https://codecov.io/github/mrc-ide/odin/coverage.svg?branch=master)](https://codecov.io/github/mrc-ide/odin?branch=master)

![](https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Odin_%28Manual_of_Mythology%29.jpg/250px-Odin_%28Manual_of_Mythology%29.jpg)

`odin` implements a high-level language for describing and implementing ordinary differential equations in R.  It provides a "domain specific language" (DSL) which _looks_ like R but is compiled directly to C.  The actual solution of the differential equations is done with the deSolve package, giving access to the excellent Livermore solvers (`lsoda`, `lsode`, etc).

**Warning: This project is in the early scoping stages; do not use for anything other than amusement/frustration purposes**

* The DSL is _declarative_ reflecting the mathematical nature of the equations (typically ODEs are simple mathematical relationships, so the order should not matter).
* It includes support for equations that involve vectors, matrices and higher dimensional arrays (up to 8!), including a high-level array indexing notation that removes the need for looping.
* Delay differential equations are supported, including when the delayed quantities are expressions of variables.
* The equations are analysed before compilation so that parts that do not depend on time are not included in the final derivative calculations.
* Supports user-supplied parameters for any part of the system.
* Supports a large number of mathematical functions

In addition, the same machinery can be used to generate discrete-time models that proceed over a set of steps (rather than through continuous time).  These may be stochastic and make use of any of R's random number functions.

`odin` works using code generation; the nice thing about this approach is that it never gets bored.  So if the generated code has lots of tedious repetitive bits, they're at least likely to be correct (compared with implementing yourself).

## Background

The "deSolve" package for R is the de-facto way of solving differential equations in R; it provides excellent solvers and has remained stable for over a decade.  However, users must implement equations in R and suffer a large speed cost, or implement their equations in C which is (depending on the complexity of the system) either routine and a bit boring, or complicated and error prone.  This translation can be especially complicated with delay differential equations, or with models where the variables are more naturally stored as variable sized arrays.

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
  config(base) <- "lorenz"
}, path)
```

The connection to the R and C versions in the section above should be fairly clear.  The code above is never actually evaluated though; intead it is parsed and used to build up C code for the model.

Note that this includes initial conditions; all odin models include specifications for initial conditions because the ordering of the varaibles is arbitrary and may be re-ordered.

This generates an object that can be used to integrate the set of differential equations, by default starting at the initial conditions specified above (though custom initial conditions can be given).  The equations are translated into C, compiled, loaded, and bundled into an object.  `lorenz` here is a function that generates an instance of the model.

```r
mod <- lorenz()
t <- seq(0, 100, length.out=50000)
y <- mod$run(t)
```

For more complicated examples, check out an [age structured SIR model](tests/testthat/examples/array_odin.R), and for more details see the [vignette](https://mrc-ide.github.io/odin/vignettes/odin.html)

# Limitations

Writing this has given me a much greater appreciation of the difficulties of writing compiler error messages.

This does not attempt to translate R into C (though very simple expressions are handled) but only a small subset that follows the sterotyped way that R+C ODE models tend to be written.  It tries to do things like minimise the number of memory allocations while preventing leaks.

The code generated by `odin` may not make sense, may not compile and may crash R.  Over time I will try to catch more of these during the parse step, but there's a lot of checking to do.  For now, be warned.

Because this is very new, it is quite possible that the code that is generated will not correspond to your model.  **You are advised to read the generated code and to test your model thoroughly before assuming it is doing the right thing.**  The generated code is designed to be straightforward to read, leaving any really funky optimisation to the compiler.

Because this relies on code generation, and the approach is partly textual, some oddities will appear in the generated code (things like `n + 0`).  Over time I'll remove the most egregious of these.  It's probable that there will be some unused variables, and unused elements in the parameters struct.  Currently the generated code passes through gcc and clang with no warnings using `-Wall -Wextra -pedantic` though over 100 examples in the unit tests; the aim is to always generate warning-free C code.

# Installation

```r
devtools::install_github("mrc-ide/odin", upgrade = FALSE)
```

You will need a working compiler.  `odin::can_compile()` will check if it is able to compile things.

This package depends on [`dde`](https://github.com/richfitz/dde) (which in turn depends on [`ring`](https://github.com/richfitz/ring)) for solving large systems of delay differential equations.
