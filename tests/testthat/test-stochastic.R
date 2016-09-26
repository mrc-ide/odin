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

test_that("array stochastic variables are time dependent", {
  ## This checks that even in the absence of array indexing on the RHS
  ## array variables are set correctly when stochastic.
  gen <- odin::odin({
    initial(x[]) <- 0
    update(x[]) <- norm_rand()
    dim(x) <- 3
  }, verbose = TEST_VERBOSE)

  mod <- gen()
  tt <- 0:20
  set.seed(1)
  yy <- mod$run(tt)
  zz <- mod$transform_variables(yy)
  set.seed(1)
  cmp <- rbind(0, matrix(rnorm(3 * 20), 20, 3, TRUE))
  expect_equal(zz$x, cmp)
})

test_that("stochastic initial conditions don't get called every step", {
  ## There is quite a few nasty little conditions that are tested
  ## here.
  gen <- odin::odin({
    v <- norm_rand() # this variable is implicitly time dependent.
    initial(x) <- v
    update(x) <- x + 1
  }, verbose = TEST_VERBOSE)

  cmp <- .Random.seed
  mod <- gen()
  expect_equal(.Random.seed, cmp)

  ## Initial conditions (why is $init even a member here?)
  expect_null(mod$init)

  ## Re-running the initial conditions gives different answers:
  x0 <- mod$initial(0L)
  expect_false(identical(.Random.seed, cmp))
  expect_true(mod$initial(0L) != x0)

  ## Run the model from scratch
  tt <- 0:20
  set.seed(1)
  yy1 <- mod$run(tt)
  z <- rnorm(1)

  ## First number drawn from distribution, leaving RNG moved forward
  ## by a single normal draw:
  set.seed(1)
  cmp <- rnorm(2)
  expect_equal(yy1[, "x"], cmp[[1]] + tt)
  expect_equal(z, cmp[[2]])

  ## Don't advance the seed if not hitting the initial conditions.
  cmp <- .Random.seed
  expect_equal(mod$run(tt, 0)[, "x"], as.numeric(0:20))
  expect_equal(mod$run(tt, 1)[, "x"], as.numeric(1:21))
  expect_equal(.Random.seed, cmp)
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

test_that("exotic stochastic functions", {
  gen <- odin::odin({
    initial(x) <- 0
    mu <- 1
    sd <- 2
    update(x) <- rnorm(mu, sd)
  }, verbose = TEST_VERBOSE)

  set.seed(1)
  mod <- gen()
  y <- mod$run(0:10)

  set.seed(1)
  expect_equal(y[-1, "x"], rnorm(10, 1, 2))
})

unload_dlls()
