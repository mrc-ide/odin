context("odin: ir based generator")

test_that("trivial model", {
  gen <- odin2({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2
  })

  mod <- gen()
  expect_is(mod, "odin_model")
  expect_equal(mod$initial(0), 1)
  expect_equal(mod$initial(10), 1)
  expect_equal(mod$deriv(0, 0), 2)
  expect_equal(mod$deriv(10, 10), 2)
  tt <- 0:10
  yy <- mod$run(tt)
  expect_equal(colnames(yy), c("t", "y"))
  expect_equal(yy[, 1], tt)
  expect_equal(yy[, 2], seq(1, length.out = length(tt), by = 2))

  expect_equal(mod$contents(), list(initial_y = 1, r = 2))
})


## This tests a few things
##
## 1. can we use time dependent rhs
## 2. can we make transient variables work correctly
## 3. we can construct somewhat nontrivial expressions
##
## This should integrate to a parabola y = 1 + t^2
test_that("Time dependent rhs", {
  gen <- odin2({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2 * t
  })

  ## This looks like a reasonable rhs but it's going through the
  ## internal storage instead of being transient.
  mod <- gen()

  tt <- 0:10
  yy <- mod$run(tt, atol = 1e-8, rtol = 1e-8)
  expect_equal(yy[, 1], tt)
  expect_equal(yy[, 2], 1 + tt^2)

  ## NOTE: this is very implementation dependent but hopefully
  ## confirms that transient variables are dealt with appropriately
  expect_equal(body(r6_private(mod)$core$rhs_dde)[[3]],
               quote(r <- 2 * t))
  expect_equal(mod$contents(), list(initial_y = 1))
})


test_that("Time dependent initial conditions", {
  gen <- odin2({
    y1 <- cos(t)
    y2 <- y1 * (r + t)
    r <- 1
    deriv(y3) <- y2
    initial(y3) <- y2
  })

  mod <- gen()

  f <- function(t) {
    cos(t) * (1 + t)
  }

  expect_equal(mod$initial(0), f(0))
  expect_equal(mod$initial(1), f(1))
  expect_equal(mod$deriv(0, 1), f(0))
  expect_equal(mod$deriv(1, 1), f(1))

  expect_equal(mod$contents(),
               list(initial_y3 = f(1), r = 1))
})


## Tests: that we can actually use state variables in a calculation
test_that("use state in derivative calculation", {
  gen <- odin2({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- 1
    K <- 100
    r <- 2.0
  })

  mod <- gen()
  ## two equilibria:
  expect_equal(mod$deriv(0, 0), 0)
  expect_equal(mod$deriv(0, 100), 0)
  ## off equilibria:
  expect_equal(mod$deriv(0, 1), 2 * 1 * 99 / 100)
})


## Tests: multiple scalar variables can be packed together properly
test_that("multiple variables", {
  gen <- odin2({
    deriv(y1) <- sigma * (y2 - y1)
    deriv(y2) <- R * y1 - y2 - y1 * y3
    deriv(y3) <- -b * y3 + y1 * y2
    initial(y1) <- 10.0
    initial(y2) <- 1.0
    initial(y3) <- 1.0
    sigma <- 10.0
    R     <- 28.0
    b     <-  8.0 / 3.0
  })
  mod <- gen()
  expect_equal(mod$initial(), c(10, 1, 1))
  expect_equal(mod$deriv(0, mod$initial()), c(-90, 269, 22 / 3))
})


## Tests: scalar user variables
test_that("user variables", {
  gen <- odin2({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user(1)
    K <- 100
    r <- user()
  })

  ## options(error = recover)
  expect_error(gen())

  expect_equal(gen(r = pi)$contents(),
               list(K = 100, N0 = 1, initial_N = 1, r = pi))
  expect_equal(gen(r = pi, N0 = 10)$contents(),
               list(K = 100, N0 = 10, initial_N = 10, r = pi))
  expect_equal(gen(r = pi, N0 = 10)$initial(), 10)
  expect_equal(gen(r = pi, N0 = 10)$deriv(0, 10),
               pi * 10 * (1 - 10 / 100))
})


## Tests: basic output
##
## This one is about as basic as I can see!
test_that("output", {
  gen <- odin2({
    deriv(y) <- 2
    initial(y) <- 1
    output(z) <- t
  })

  tt <- 0:10

  mod <- gen()

  expect_equal(mod$deriv(0, 1), structure(2, output = 0))
  expect_equal(mod$deriv(10, 1), structure(2, output = 10))

  yy1 <- mod$run(tt)
  expect_equal(colnames(yy1), c("t", "y", "z"))
  expect_equal(yy1[, "t"], tt)
  expect_equal(yy1[, "y"], seq(1, length.out = length(tt), by = 2))
  expect_equal(yy1[, "z"], tt)

  yy2 <- gen(TRUE)$run(tt)
  expect_equal(colnames(yy2), c("t", "y", "z"))
  expect_equal(yy2[, "t"], tt)
  expect_equal(yy2[, "y"], seq(1, length.out = length(tt), by = 2))
  expect_equal(yy2[, "z"], tt)
})


## Do some nontrivial calculation in the output
test_that("output", {
  gen <- odin2({
    deriv(y) <- 2
    initial(y) <- 1
    output(z) <- a * 2
    a <- t + y
  })

  mod <- gen()
  expect_equal(mod$deriv(0, 1), structure(2, output = 2))
  expect_equal(mod$deriv(10, 1), structure(2, output = 22))
})


## Basic discrete models
test_that("discrete", {
  gen <- odin2({
    initial(x) <- 1
    update(x) <- x + 1
  })
  mod <- gen()

  expect_equal(mod$initial(), 1)
  expect_equal(mod$update(0, 1), 2)

  tt <- 0:10
  yy <- mod$run(tt)
  expect_equal(yy, cbind(step = tt, x = tt + 1))
})
