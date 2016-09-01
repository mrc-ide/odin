context("delays")

test_that("mixed delay model", {
  ## I want a model where the components of a delay are an array and a
  ## scalar.  This is going to be a pretty common thing to have, and I
  ## think it will throw up a few corner cases that are worth keeping
  ## track of.
  ##
  ## At the same time this will pick up user sized delayed arrays.

  gen <- odin::odin({
    ## Exponential growth/decay of 'y'
    deriv(y[]) <- r[i] * y[i]
    initial(y[]) <- y0[i]
    r[] <- user()

    ## Drive the system off of given 'y0'
    y0[] <- user()
    dim(y0) <- user()
    dim(y) <- length(y0)
    dim(r) <- length(y0)

    ## And of a scalar 'z'
    deriv(z) <- rz * z
    initial(z) <- 1
    rz <- 0.1

    ## Sum over all the variables
    total <- sum(y) + z

    ## Delay the total of all variables
    a <- delay(total, 2.5)

    ## And output that for checking
    output(a) <- a
  }, verbose = TEST_VERBOSE)

  for (i in 1:2) {
    y0 <- runif(3)
    r <- runif(3)
    if (i == 1) {
      mod <- gen(y0 = y0, r = r)
    } else {
      mod$set_user(y0 = y0, r = r)
    }

    tt <- seq(0, 5, length.out = 101)
    real_y <- t(y0 * exp(outer(r, tt)))
    real_z <- exp(0.1 * tt)

    yy <- mod$run(tt, rtol = 1e-8, atol = 1e-8)
    zz <- mod$transform_variables(yy)
    expect_equal(zz$y, real_y, tolerance = 1e-6)
    expect_equal(zz$z, real_z, tolerance = 1e-6)

    dat <- mod$contents()
    ## Storing all the initial conditions correctly:
    expect_equal(dat$initial_t, 0)
    expect_equal(dat$initial_y, y0)
    expect_equal(dat$initial_z, 1.0)

    i <- tt > 2.5
    real_a <- rep(sum(y0) + 1, length(i))
    real_a[i] <- rowSums(t(y0 * exp(outer(r, tt[i] - 2.5)))) +
      exp(0.1 * (tt[i] - 2.5))

    expect_equal(zz$a, real_a, tolerance = 1e-6)
  }
})

test_that("missing variables in delay", {
  expect_error(odin::odin({
    ylag <- delay(x, 10)
    initial(y) <- 0.5
    deriv(y) <- y + ylag
  }), "Missing variable in delay expression")

  expect_error(odin::odin({
    ylag <- delay(x + y, 10)
    initial(y) <- 0.5
    deriv(y) <- y + ylag
  }), "Missing variable in delay expression")

  expect_error(odin::odin({
    ylag <- delay(x + z, 10)
    initial(y) <- 0.5
    deriv(y) <- y + ylag
  }), "Missing variables in delay expression")
})
