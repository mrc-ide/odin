context("discrete")

test_that("basic", {
  gen <- odin::odin({
    initial(x) <- 1
    update(x) <- x + 1
  }, verbose = TEST_VERBOSE)

  mod <- gen()
  expect_equal(mod$contents(), list(initial_x = 1))
  y0 <- mod$initial()
  expect_equal(y0, 1.0)
  expect_equal(mod$update(0L, y0), 2.0)

  tt <- 0:10
  res <- mod$run(tt)

  expect_equal(res, cbind(step = tt, x = 1:11))
})

test_that("output", {
  gen <- odin::odin({
    initial(x[]) <- x0[i]
    update(x[]) <- x[i] + r[i]
    x0[] <- user()
    r[] <- user()
    dim(x0) <- user()
    dim(x) <- length(x0)
    dim(r) <- length(x)
    output(total) <- sum(x)
  }, verbose = TEST_VERBOSE)

  x0 <- runif(10)
  r <- runif(length(x0))

  mod <- gen(x0 = x0, r = r)

  tt <- 0:10
  yy <- mod$run(tt)
  zz <- mod$transform_variables(yy)

  expect_equal(zz$x, t(outer(r, tt) + x0))
  expect_equal(zz$total, rowSums(zz$x))
})

test_that("delays", {
  gen <- odin::odin({
    initial(y) <- 1
    update(y) <- y + yprev
    yprev <- delay(y, 1)
  }, verbose = TEST_VERBOSE)

  mod <- gen()
  expect_null(mod$update) # no update function in a delay model

  tt <- seq(0:10)
  yy <- mod$run(tt)
  expect_equal(yy[, "y"], c(1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144))
})

test_that("interpolate", {
  gen <- odin::odin({
    initial(x) <- 0
    update(x) <- x + pulse
    pulse <- interpolate(sp, zp, "constant")
    sp[] <- user()
    zp[] <- user()
    dim(sp) <- user()
    dim(zp) <- length(sp)
  }, verbose = TEST_VERBOSE)

  sp <- c(0, 10, 20)
  zp <- c(0, 1, 0)
  expect_error(gen(sp=sp, zp=zp[1:2]), "Expected length 3 value for zp")
  expect_error(gen(sp=sp, zp=rep(zp, 2)), "Expected length 3 value for zp")

  mod <- gen(sp=sp, zp=zp)

  tt <- 0:30
  expect_error(mod$run(tt - 1L),
               "Integration times do not span interpolation")

  yy <- mod$run(tt)
  zz <- cumsum(ifelse(tt <= 10 | tt > 20, 0, 1))
  expect_equal(yy[, 2], zz)
})

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

test_that("disallow stochastic functions in ODEs", {
  ## Here's a stochastic random walk:
  expect_error(odin::odin({
    initial(x) <- 0
    deriv(x) <- x + norm_rand()
  }), "Stochastic functions not allowed in ODE models")
})
