context("run: %TARGET%: continuous delays")

test_that("mixed delay model", {
  ## I want a model where the components of a delay are an array and a
  ## scalar.  This is going to be a pretty common thing to have, and I
  ## think it will throw up a few corner cases that are worth keeping
  ## track of.
  ##
  ## At the same time this will pick up user sized delayed arrays.

  gen <- odin({
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
  })

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


test_that("use subset of variables", {
  gen <- odin({
    deriv(a) <- 1
    deriv(b) <- 2
    deriv(c) <- 3
    initial(a) <- 0.1
    initial(b) <- 0.2
    initial(c) <- 0.3
    ## TODO: output(tmp) <- delay(...) should be ok
    tmp <- delay(b + c, 2)
    output(tmp) <- tmp
  })

  tt <- seq(0, 10, length.out = 101)
  mod <- gen()
  yy <- mod$run(tt)
  expect_equal(yy[, "tmp"],
               0.5 + ifelse(tt <= 2, 0, (tt - 2)) * 5)
})

test_that("delay array storage", {
  gen <- odin({
    ## Exponential growth/decay of 'y'
    deriv(y[]) <- r[i] * y[i]
    initial(y[]) <- y0[i]
    r[] <- user()

    ## Drive the system off of given 'y0'
    y0[] <- user()
    dim(y0) <- user()
    dim(y) <- length(y0)
    dim(r) <- length(y0)

    y2[] <- y[i] * y[i]
    total2 <- sum(y2)
    dim(y2) <- length(y0)

    ## Delay the total of all variables
    a <- delay(total2, 2.5)

    ## And output that for checking
    output(a) <- a
  })

  for (i in 1:2) {
    y0 <- runif(2 + i)
    r <- runif(2 + i)
    if (i == 1) {
      mod <- gen(y0 = y0, r = r)
    } else {
      mod$set_user(y0 = y0, r = r)
    }

    tt <- seq(0, 5, length.out = 101)
    real_y <- t(y0 * exp(outer(r, tt)))

    yy <- mod$run(tt, rtol = 1e-8, atol = 1e-8)
    zz <- mod$transform_variables(yy)

    expect_equal(zz$y, real_y, tolerance = 1e-6)

    dat <- mod$contents()
    expect_true("delay_state_a" %in% names(dat))

    i <- tt > 2.5
    real_a <- rep(sum(y0^2), length(i))
    real_a[i] <- rowSums(t(y0 * exp(outer(r, tt[i] - 2.5)))^2)

    expect_equal(zz$a, real_a, tolerance = 1e-6)
  }
})

test_that("3 arg delay", {
  gen <- odin({
    ylag <- delay(y, 3, 2) # lag time 3, default value 2
    initial(y) <- 0.5
    deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
    output(ylag) <- ylag
    config(base) <- "delay3"
  })

  mod <- gen()
  tt <- seq(0, 3, length.out = 101)
  yy <- mod$run(tt)
  expect_equal(yy[, "ylag"], rep(2.0, length(tt)))

  tt <- seq(0, 10, length.out = 101)
  yy <- mod$run(tt)

  ylag <- yy[, "ylag"]
  expect_true(all(ylag[tt <= 3] == 2))
  expect_true(all(ylag[tt > 3] < 1)) # quite a big jump at first
})


test_that("3 arg delay with array", {
  gen <- odin({
    deriv(a[]) <- i
    initial(a[]) <- (i - 1) / 10
    dim(a) <- 5
    alt[] <- user()
    dim(alt) <- length(a)
    tmp[] <- delay(a[i], 2, alt[i])
    dim(tmp) <- length(a)
    output(tmp[]) <- TRUE # or tmp[i]
  })

  tt <- seq(0, 2, length.out = 11)
  x <- -runif(5, 2, 3)
  mod <- gen(alt = x)
  yy <- mod$transform_variables(mod$run(tt))
  expect_equal(yy$tmp, matrix(x, length(tt), length(x), TRUE))

  tt <- seq(0, 10, length.out = 101)
  yy <- mod$transform_variables(mod$run(tt))
  i <- tt <= 2

  expect_equal(yy$tmp[i, ], matrix(x, sum(i), length(x), TRUE))
  expect_equal(yy$tmp[!i, ],
               t(outer(1:5, tt[!i] - 2) + (0:4) / 10))
})


## This should also be done with a couple of scalars thrown in here
## too I think; they change things also.
test_that("delay index packing", {
  gen <- odin({
    deriv(a[]) <- i
    deriv(b[]) <- i
    deriv(c[]) <- i
    deriv(d[]) <- i
    deriv(e[]) <- i

    initial(a[]) <- 1
    initial(b[]) <- 2
    initial(c[]) <- 3
    initial(d[]) <- 4
    initial(e[]) <- 5

    foo[] <- delay(b[i] + c[i + 1] + e[i + 2], 2)
    output(foo[]) <- TRUE

    dim(foo) <- 9
    dim(a) <- 10
    dim(b) <- 11
    dim(c) <- 12
    dim(d) <- 13
    dim(e) <- 14
  })

  mod <- gen()
  dat <- mod$contents()

  seq0 <- function(n) seq_len(n)

  expect_equal(dat$dim_delay_foo, dat$dim_b + dat$dim_c + dat$dim_e)

  delay_index_foo <- c(dat$dim_a + seq0(dat$dim_b),
                       dat$offset_variable_c + seq0(dat$dim_c),
                       dat$offset_variable_e + seq0(dat$dim_e))
  if (odin_target_name() == "c") {
    delay_index_foo <- delay_index_foo - 1L
  }
  expect_equal(dat$delay_index_foo, delay_index_foo)

  tt <- seq(0, 10, length.out = 11)
  yy <- mod$transform_variables(mod$run(tt))

  i <- seq_len(dat$dim_foo)
  expect_equal(yy$foo[1, ],
               yy$b[1, i] + yy$c[1, i + 1] + yy$e[1, i + 2])
  expect_equal(yy$foo[8, ],
               yy$b[6, i] + yy$c[6, i + 1] + yy$e[6, i + 2])
})


test_that("nontrivial time", {
  gen <- odin({
    ylag <- delay(y, 2 + 3)
    initial(y) <- 0.5
    deriv(y) <- 1
    output(ylag) <- TRUE
  })

  tt <- 0:10
  y <- gen()$run(tt)
  expect_equal(y[, "ylag"],
               c(rep(0.5, 6), seq(1.5, by = 1, length.out = 5)))
})


test_that("overlapping array storage", {
  gen <- odin({
    ## Exponential growth/decay of 'y'
    deriv(y[]) <- r[i] * y[i]
    initial(y[]) <- y0[i]
    r[] <- user()

    ## Drive the system off of given 'y0'
    y0[] <- user()
    dim(y0) <- user()
    dim(y) <- length(y0)
    dim(r) <- length(y0)

    y2[] <- y[i] * y[i]
    total2 <- sum(y2)
    dim(y2) <- length(y0)

    ## Delay the total of all variables
    a <- delay(total2, 2.5)
    b <- delay(total2, 3)

    ## And output that for checking
    output(a) <- a
    output(b) <- b
  })

  for (i in 1:2) {
    y0 <- runif(2 + i)
    r <- runif(2 + i)
    if (i == 1) {
      mod <- gen(y0 = y0, r = r)
    } else {
      mod$set_user(y0 = y0, r = r)
    }

    tt <- seq(0, 5, length.out = 101)
    real_y <- t(y0 * exp(outer(r, tt)))

    yy <- mod$run(tt, rtol = 1e-8, atol = 1e-8)
    zz <- mod$transform_variables(yy)

    expect_equal(zz$y, real_y, tolerance = 1e-6)

    dat <- mod$contents()
    expect_true("delay_state_a" %in% names(dat))

    i <- tt > 2.5
    real_a <- rep(sum(y0^2), length(i))
    real_a[i] <- rowSums(t(y0 * exp(outer(r, tt[i] - 2.5)))^2)

    i <- tt > 3
    real_b <- rep(sum(y0^2), length(i))
    real_b[i] <- rowSums(t(y0 * exp(outer(r, tt[i] - 3)))^2)

    expect_equal(zz$a, real_a, tolerance = 1e-6)
    expect_equal(zz$b, real_b, tolerance = 1e-6)
  }
})


test_that("delayed delays", {
  gen <- odin({
    deriv(y) <- y
    initial(y) <- 1

    b <- delay(c, 3)
    c <- a + 1
    a <- delay(y + 1, 2)
    output(a) <- TRUE
    output(b) <- TRUE
  })

  tt <- seq(0, 7, length.out = 51)
  yy <- gen()$run(tt)

  ## First delay
  a <- ifelse(tt > 2, exp(tt - 2) + 1, exp(0) + 1)
  b <- ifelse(tt > 5, exp(tt - 5) + 2, exp(0) + 2)

  expect_equal(yy[, "a"], a, tolerance = 1e-6)
  expect_equal(yy[, "b"], b, tolerance = 1e-6)
})


test_that("compute derivative", {
  gen <- odin({
    deriv(a) <- sin(t)
    initial(a) <- -1

    pi <- asin(1) * 2
    x <- delay(a, pi / 4)
    deriv(b) <- x
    initial(b) <- 0
    output(x) <- TRUE
  })

  ## The analytic solution here is:
  ##   a = -cos(t)
  ##   b = if t <= pi / 4 => -t
  ##       else           => cos(t + pi / 4) - pi / 4
  ##   x = if t <= pi / 4 => -1
  ##       else           => -cos(t - pi / 4)
  ##
  ## The derivative calculations are:
  ##
  ##   da/dt = sin(t)
  ##   db/dt = if initialised => -cos(t)
  ##           else           => -1
  mod <- gen()
  expect_identical(mod$contents()$initial_t, NA_real_)

  ## First, uninitialised:
  t0 <- 0
  y0 <- mod$initial(t0)
  expect_equal(y0, c(-1, 0))
  expect_equal(mod$deriv(t0, y0),
               structure(c(0, -1), output = -1))
  expect_identical(mod$contents()$initial_t, NA_real_)

  ## end of the delay
  t1 <- pi / 4
  y1 <- c(-cos(t1), -t1)
  expect_equal(mod$deriv(t1, y1),
               structure(c(sin(t1), -1), output = -1))
  expect_identical(mod$contents()$initial_t, NA_real_)

  ## after the delay
  t2 <- pi / 2
  y2 <- c(-cos(t2), -t2)
  expect_equal(mod$deriv(t2, y2),
               structure(c(sin(t2), -1), output = -1))
  expect_identical(mod$contents()$initial_t, NA_real_)

  ## Then run the model
  t <- seq(0, 4 * pi, length.out = 101)
  y <- mod$run(t)

  ## First, uninitialised:
  expect_equal(mod$deriv(t0, y0),
               structure(c(0, -1), output = -1))
  expect_identical(mod$contents()$initial_t, 0)

  ## end of the delay
  expect_equal(mod$deriv(t1, y1),
               structure(c(sin(t1), -1), output = -1))
  expect_identical(mod$contents()$initial_t, 0)

  ## after the delay
  y2 <- c(-cos(t2), cos(t2 + pi / 4) - pi / 4)
  expect_equal(mod$deriv(t2, y2),
               structure(c(sin(t2), -cos(t2 - pi / 4)),
                         output = -cos(t2 - pi / 4)),
               tolerance = 1e-5)
})
