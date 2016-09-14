context("stochastic")

test_that("stochastic", {
  ## Here's a stochastic random walk:
  gen <- odin::odin({
    initial(x) <- 0
    update(x) <- x + norm_rand()
  }, verbose = TEST_VERBOSE)

  mod <- gen()
  tt <- 0:20
  set.seed(1)
  yy1 <- mod$run(tt)

  set.seed(1)
  cmp <- rnorm(length(tt) - 1L)
  expect_equal(cumsum(c(0, cmp)), yy1[, "x"])

  ## Repeatable
  set.seed(1)
  yy2 <- mod$run(tt)
  expect_equal(yy1, yy2)
})

## I'm not totally sure what the right call is here.  If I make a
## variable that is used only in the initial condition I do not want
## that repeatedly called during the run.
test_that("stochastic variables are time dependent", {
  gen <- odin::odin({
    v <- norm_rand() # this variable is implicitly time dependent.
    initial(x) <- 0
    update(x) <- x + v
  }, verbose = TEST_VERBOSE)

  mod <- gen()
  tt <- 0:20
  set.seed(1)
  yy1 <- mod$run(tt)

  set.seed(1)
  cmp <- rnorm(length(tt) - 1L)
  expect_equal(cumsum(c(0, cmp)), yy1[, "x"])
})

test_that("disallow stochastic functions in ODEs", {
  ## Here's a stochastic random walk:
  expect_error(odin::odin({
    initial(x) <- 0
    deriv(x) <- x + norm_rand()
  }), "Stochastic functions not allowed in ODE models")
})

## This is not allowed directly, though we may allow some via a two
## step process perhaps.
test_that("disallow stochastic functions on array rhs", {
  expect_error(
    odin::odin({
      initial(x[]) <- 1
      dim(x) <- 10
      update(x[runif(1, 10)]) <- 2
    }, verbose = TEST_VERBOSE),
    "Invalid array use on lhs")
})
