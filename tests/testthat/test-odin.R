context("odin")

## Tests of the approach against some known models.
test_that("constant model", {
  gen <- odin::odin({
    deriv(y) <- 0.5
    initial(y) <- 1
  }, verbose=FALSE)
  mod <- gen()
  expect_identical(mod$init, 1.0)
  expect_identical(mod$deriv(0.0, mod$init), 0.5)

  tt <- seq(0, 10, length.out=11)
  yy <- mod$run(tt)
  expect_equal(yy[, 2L], seq(1.0, length.out=length(tt), by=0.5))
})

test_that("user variables", {
  gen <- odin::odin({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user(1)
    K <- user(100)
    r <- user()
  }, verbose=FALSE)

  ## Two different errors when r is not provided:
  expect_error(gen(), 'argument "r" is missing')
  expect_error(gen(NA_real_), "Expected value for r")

  mod <- gen(pi)
  dat <- mod$contents()
  expect_equal(dat$r, pi)
  expect_equal(dat$N0, 1.0)
  expect_equal(dat$K, 100.0)

  ## This should be a noop:
  mod$set_user()
  dat <- mod$contents()
  expect_equal(dat$r, pi)
  expect_equal(dat$N0, 1.0)
  expect_equal(dat$K, 100.0)

  ## Now, try setting one of these:
  ## TODO:
  ##   mod$set_user(list(N0=5))
  ## gives the entirely unuseful error message:
  ##   STRING_ELT() can only be applied to a 'character vector', not a 'NULL'
  mod$set_user(N0=5)
  dat <- mod$contents()
  expect_equal(dat$r, pi)
  expect_equal(dat$N0, 5.0)
  expect_equal(dat$K, 100.0)

  ## Don't reset to default on subsequent set:
  mod$set_user()
  expect_equal(mod$contents()$N0, 5.0)
})

test_that("non-numeric time", {
  ## Only an issue for delay models or models with time-dependent
  ## initial conditions.
  gen <- odin::odin({
    ylag <- delay(y, 10)
    initial(y) <- 0.5
    deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
  }, verbose=FALSE)
  mod <- gen()
  t <- as.integer(0:10)
  expect_silent(mod$run(t))
})

test_that("non-numeric user", {
  gen <- odin::odin({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user(1)
    K <- user(100)
    r <- user()
  }, verbose=FALSE)
  mod <- gen(1L)
  expect_is(mod$contents()$r, "numeric")
  expect_identical(mod$contents()$r, 1.0)
})

test_that("conditionals", {
  gen <- odin::odin({
    deriv(x) <- if (x > 2) 0 else 0.5
    initial(x) <- 0
  }, verbose=FALSE)

  ## Hey ho it works:
  mod <- gen()
  t <- seq(0, 5, length.out=101)
  y <- mod$run(t)

  expect_equal(y[, 2], ifelse(t < 4, t * 0.5, 2.0), tolerance=1e-5)
})

test_that("conditionals, precendence", {
  gen <- odin::odin({
    deriv(x) <- 0.1 + 2 * if (t > 2) -0.1 else 0.5
    initial(x) <- 0
  }, verbose=FALSE)

  mod <- gen()
  t <- seq(0, 5, length.out=101)
  y <- mod$run(t)

  cmp <- ifelse(t < 2, 1.1 * t, 2.4 -0.1 * t)
  expect_equal(y[, 2], cmp, tolerance=1e-5)
})

test_that("time dependent", {
  ## A time dependent initial condition:
  gen_t <- odin::odin({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- sqrt(t) + 1
    K <- 100
    r <- 0.5
  }, verbose=FALSE)

  ## The same model, but taking N0 as a user parameter.
  gen_cmp <- odin::odin({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user()
    K <- 100
    r <- 0.5
  }, verbose=FALSE)

  mod_t <- gen_t()
  t0 <- seq(0,  10, length.out = 101)
  t1 <- seq(10, 10, length.out = 101)

  expect_equal(mod_t$run(t0), gen_cmp(sqrt(t0[[1]]) + 1)$run(t0))
  expect_equal(mod_t$run(t1), gen_cmp(sqrt(t1[[1]]) + 1)$run(t1))
})

test_that("time dependent initial conditions", {
  gen <- odin::odin({
    y1 <- sin(t)
    deriv(y2) <- y1
    initial(y2) <- -1
    output(y1) <- y1
  }, verbose=FALSE)

  mod <- gen()
  t <- seq(0, 2 * pi, length.out=101)
  y <- mod$run(t, atol=1e-8, rtol=1e-8)
  expect_identical(y[, 3L], sin(t))
  expect_equal(y[, 2L], cos(t + pi), tolerance=1e-6)
})

test_that("user c", {
  ## The config(include) interface will probably change because it's
  ## pretty nasty.
  gen <- odin({
    config(include) <- "user_fns.c"
    z <- squarepulse(t, 1, 2)
    output(z) <- z
    deriv(y) <- z
    initial(y) <- 0
  }, verbose=FALSE)

  mod <- gen()
  t <- seq(0, 3, length.out=301)
  y <- mod$run(t)

  expect_equal(y[, 3L], as.numeric(t >= 1 & t < 2))
  cmp <- -1 + t
  cmp[t < 1] <- 0
  cmp[t > 2] <- 1
  expect_equal(y[, 2L], cmp, tolerance=1e-5)
})

test_that("time dependent initial conditions", {
  ## This works OK except that I've generated
  gen <- odin::odin({
    y1 <- cos(t)
    y2 <- y1 * (1 + t)
    deriv(y3) <- y2
    initial(y3) <- y2
    output(y1) <- y1
    output(y2) <- y2
  }, verbose=FALSE)

  mod <- gen()

  ## Initial conditions get through here:
  expect_equal(mod$initial(0), 1, check.attributes=FALSE)
  expect_equal(mod$initial(1), cos(1) * 2,
               check.attributes=FALSE)

  t <- seq(0, 4 * pi, length.out=101)
  y <- mod$run(t, atol=1e-8, rtol=1e-8)
  expect_equal(as.vector(y[1, 2]), 1.0)
  ## TODO: Compute analytic expectation and compare here.
  expect_equal(as.vector(y[length(t), 2]), 1.0, tolerance=1e-7)
})

test_that("time dependent initial conditions depending on vars", {
  gen <- odin::odin({
    v1 <- exp(-t)

    initial(y1) <- 1
    deriv(y1) <- y1 * v1

    deriv(y2) <- y2 * 0.5
    initial(y2) <- y1 + v1

    deriv(y3) <- y3 * 0.1
    initial(y3) <- y1 + y2
  }, verbose=FALSE)

  mod <- gen()
  expect_equal(mod$initial(0), c(1, 2, 3))
  expect_equal(mod$initial(1), c(1, 1 + exp(-1), 2 + exp(-1)))
})

## This test case kindly contributed by @blackedder in #14
test_that("unused variable in output", {
  gen <- odin::odin({
    initial(S) <- N - I0
    initial(E1) <- 0
    initial(E2) <- 0
    initial(I1) <- I0
    initial(I2) <- 0
    initial(R) <- 0

    N <- 1e7
    I0 <- 1

    lambda <- 0.00001 * (I1 + I2)
    gamma1 <- 2.5
    gamma2 <- 1.1

    deriv(S) <- -lambda * S
    deriv(E1) <- lambda * S - gamma1 * E1
    deriv(E2) <- gamma1 * (E1 - E2)
    deriv(I1) <- gamma1 * E2  - gamma2 * I1
    deriv(I2) <- gamma2 * (I1 - I2)
    deriv(R) <- gamma2 * I2

    output(tot) <- S + E1 + E2 + I1 + I2 + R
  }, verbose=TRUE)
  mod <- gen()
  expect_is(mod, "ode_system")
  t <- seq(0, 10, length.out = 100)
  expect_error(mod$run(t), NA)
})
