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

  expect_equal(mod$contents(), sort_list(list(initial_y = 1, r = 2)))
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
               sort_list(list(initial_y3 = f(1), r = 1)))
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

  expect_error(gen())
  expect_error(gen(NULL),
               "Expected a value for 'r'", fixed = TRUE)

  expect_equal(gen(r = pi)$contents(),
               sort_list(list(K = 100, N0 = 1, initial_N = 1, r = pi)))
  expect_equal(gen(r = pi, N0 = 10)$contents(),
               sort_list(list(K = 100, N0 = 10, initial_N = 10, r = pi)))
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


test_that("discrete with output", {
  gen <- odin2({
    initial(x) <- 1
    update(x) <- x + 1
    output(y) <- x + step
  })
  mod <- gen()

  expect_equal(mod$update(2, 3), structure(4, output = 5))
  tt <- 0:10
  yy <- mod$run(tt)
  expect_equal(yy, cbind(step = tt, x = tt + 1, y = 2 * tt + 1))
})


## Fairly minimal array model, though it does mix array and non array
## variables, plus an array support variable.
test_that("array support", {
  gen <- odin2({
    initial(x[]) <- 1
    initial(y) <- 2
    deriv(x[]) <- r[i]
    deriv(y) <- n
    r[] <- i
    n <- 3
    dim(r) <- n
    dim(x) <- n
  })

  mod <- gen()

  ## internal data is ok:
  expect_equal(mod$contents(),
               sort_list(list(dim_r = 3, dim_x = 3, initial_x = rep(1, 3),
                              initial_y = 2, n = 3, r = 1:3)))
  expect_equal(mod$initial(), c(2, 1, 1, 1))
  expect_equal(mod$deriv(0, c(2, 1, 1, 1)), c(3, 1, 2, 3))

  tt <- 0:10
  yy <- mod$run(tt)
  expect_equivalent(yy[, ],
                    cbind(tt, 2 + tt * 3, 1 + tt, 1 + tt * 2, 1 + tt * 3,
                          deparse.level = 0))
  expect_equal(colnames(yy), c("t", "y", "x[1]", "x[2]", "x[3]"))

  expect_equal(mod$transform_variables(yy),
               list(t = tt,
                    y = yy[, 2],
                    x = unname(yy[, 3:5])))
})


test_that("multi-line array expression", {
  gen <- odin2({
    initial(x) <- 1
    deriv(x) <- 1
    a[1] <- 1
    a[2] <- 1
    a[3:n] <- a[i - 1] + a[i - 2]
    dim(a) <- n
    n <- 10
  })
  expect_equal(gen()$contents()$a, c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55))
})


test_that("3d array", {
  gen <- odin2({
    initial(y[, , ]) <- 1
    deriv(y[, , ]) <- y[i, j, k] * 0.1
    dim(y) <- c(2, 3, 4)
  })

  mod <- gen()
  d <- mod$contents()
  expect_equal(d$initial_y, array(1, c(2, 3, 4)))
  expect_equal(d$dim_y, 24)
  expect_equal(d$dim_y_1, 2)
  expect_equal(d$dim_y_2, 3)
  expect_equal(d$dim_y_3, 4)
  expect_equal(d$dim_y_12, 6)

  expect_equal(mod$initial(), rep(1, 24))
  expect_equal(mod$deriv(0, mod$initial()), rep(0.1, 24))

  tt <- 0:10
  yy <- mod$run(tt, atol = 1e-8, rtol = 1e-8)
  expect_equal(colnames(yy)[[12]], "y[1,3,2]")
  expect_equal(yy[, 1], tt)
  expect_equal(unname(yy[, -1]), matrix(rep(exp(0.1 * tt), 24), 11))
})


## User provided, constant defined arrays
test_that("user array", {
  gen <- odin2({
    initial(x[]) <- 1
    deriv(x[]) <- r[i]
    r[] <- user()
    n <- 3
    dim(r) <- n
    dim(x) <- n
  })

  mod <- gen(1:3)
  expect_identical(mod$contents()$r, as.numeric(1:3))
  expect_error(gen(1), "Expected length 3 value for r")
})


test_that("user matrix", {
  gen <- odin2({
    initial(y[, ]) <- 1
    deriv(y[, ]) <- y[i, j] * r[i, j]
    dim(y) <- c(2, 3)
    dim(r) <- c(2, 3)
    r[, ] <- user()
  })

  r <- matrix(runif(6), 2, 3)
  mod <- gen(r)
  expect_identical(mod$contents()$r, r)

  msg <- "Expected a numeric array with dimensions 2 * 3 for 'r'"

  expect_error(gen(c(r)), msg, fixed = TRUE)
  expect_error(gen(t(r)), msg, fixed = TRUE)
  expect_error(gen(r[2, 2]), msg, fixed = TRUE)
  expect_error(gen(array(1, 2:4)), msg, fixed = TRUE)
  expect_error(gen(1), msg, fixed = TRUE)
})


test_that("user array - indirect", {
  gen <- odin2({
    initial(x[]) <- 1
    deriv(x[]) <- r[i]
    r[] <- user()
    dim(r) <- n
    dim(x) <- n
    n <- user()
  })

  mod <- gen(n = 3, r = 1:3)
  expect_equal(mod$contents(),
               sort_list(list(
                 dim_r = 3,
                 dim_x = 3,
                 initial_x = rep(1, 3),
                 n = 3,
                 r = 1:3)))

  expect_error(gen(n = 4, r = 1:3),
               "Expected length 4 value for r")
})


test_that("user array - direct", {
  gen <- odin2({
    initial(x[]) <- 1
    deriv(x[]) <- r[i]
    r[] <- user()
    dim(r) <- user()
    dim(x) <- length(r)
  })

  mod <- gen(r = 1:3)
  expect_equal(
    mod$contents(),
    sort_list(list(dim_r = 3, dim_x = 3, initial_x = rep(1, 3), r = 1:3)))
  expect_error(gen(r = matrix(1, 2, 3)),
               "Expected a numeric vector for 'r'")
})


test_that("user array - direct 3d", {
  gen <- odin2({
    initial(y) <- 1
    deriv(y) <- 1
    r[, , ] <- user()
    dim(r) <- user()
  })

  m <- array(runif(24), 2:4)
  mod <- gen(m)
  expect_equal(mod$contents(),
               sort_list(list(dim_r = 24, dim_r_1 = 2, dim_r_12 = 6,
                              dim_r_2 = 3, dim_r_3 = 4, initial_y = 1,
                              r = m)))

  expect_error(gen(1), "Expected a numeric array of rank 3 for 'r'")
  expect_error(gen(matrix(1)), "Expected a numeric array of rank 3 for 'r'")
})


## NOTE: this is the test from test-interpolation.R
test_that("interpolation", {

  gen <- odin2({
    deriv(y) <- pulse
    initial(y) <- 0
    ##
    pulse <- interpolate(tp, zp, "constant")
    ##
    tp[] <- user()
    zp[] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    output(p) <- pulse
  })

  tt <- seq(0, 3, length.out = 301)
  tp <- c(0, 1, 2)
  zp <- c(0, 1, 0)
  mod <- gen(tp = tp, zp = zp)
  dat <- mod$contents()

  expect_equal(sort(names(dat)),
               sort(c("dim_tp", "dim_zp", "initial_y", "interpolate_pulse",
                      "tp", "zp")))
  ## Interpolating function works
  pulse <- cinterpolate::interpolation_function(tp, zp, "constant")(tt)
  expect_equal(vnapply(tt, dat$interpolate_pulse), pulse)

  yy <- mod$run(tt)
  zz <- ifelse(tt < 1, 0, ifelse(tt > 2, 1, tt - 1))
  expect_equal(yy[, "y"], zz, tolerance = 1e-5)
  expect_equal(yy[, "p"], pulse)
})


test_that("stochastic", {
  gen <- odin2({
    initial(x) <- 0
    update(x) <- x + norm_rand()
  })
  mod <- gen()
  expect_equal(mod$initial(), 0)

  set.seed(1)
  x <- rnorm(3)

  set.seed(1)
  y <- replicate(3, mod$update(0, 0))

  expect_identical(x, y)
})


test_that("multiple arrays: constant", {
  gen <- odin2({
    initial(x[]) <- 1
    initial(y[]) <- 2
    deriv(x[]) <- r[i]
    deriv(y[]) <- r[i]
    r[] <- i
    n <- 3
    dim(r) <- n
    dim(x) <- n
    dim(y) <- n
  })

  mod <- gen()
  expect_equal(mod$contents()$offset_y, 3)
  expect_equal(mod$initial(0), rep(1:2, each = 3))
  expect_equal(mod$deriv(0, mod$initial(0)), rep(1:3, 2))
})


test_that("multiple arrays: dynamic", {
  gen <- odin2({
    initial(x[]) <- 1
    initial(y[]) <- 2
    deriv(x[]) <- r[i]
    deriv(y[]) <- r[i]
    r[] <- i
    n <- user()
    dim(r) <- n
    dim(x) <- n
    dim(y) <- n
  })

  mod <- gen(4)
  expect_equal(mod$contents()$offset_y, 4)
  expect_equal(mod$initial(0), rep(1:2, each = 4))
  expect_equal(mod$deriv(0, mod$initial(0)), rep(1:4, 2))
})


test_that("multiple output arrays", {
  gen <- odin2({
    deriv(y[]) <- y[i] * r[i]
    initial(y[]) <- i
    dim(y) <- 3
    dim(r) <- 3
    r[] <- user()
    output(yr[]) <- y[i] / i
    dim(yr) <- 3
    output(r[]) <- TRUE
  })

  r <- runif(3)
  mod <- gen(r = r)

  expect_equal(mod$initial(0), 1:3)
  expect_equal(
    mod$deriv(0, mod$initial(0)),
    structure(1:3 * r, output = c(r, 1:3 / 1:3)))

  tt <- seq(0, 10, length.out = 101)
  yy <- mod$run(tt, atol = 1e-8, rtol = 1e-8)
  zz <- mod$transform_variables(yy)

  expect_equal(zz$y, t(1:3 * exp(outer(r, tt))), tolerance = 1e-6)
  expect_equal(zz$r, matrix(r, length(tt), 3, TRUE))
  expect_equal(zz$yr, t(t(zz$y) / (1:3)))
})


test_that("3d array time dependent and variable", {
  gen <- odin2({
    initial(y[, , ]) <- 1
    deriv(y[, , ]) <- y[i, j, k] * r[i, j, k]
    dim(y) <- c(2, 3, 4)
    dim(r) <- c(2, 3, 4)
    r[, , ] <- t * 0.1
  })

  mod <- gen()
  d <- mod$contents()
  expect_equal(d$initial_y, array(1, c(2, 3, 4)))
  expect_equal(d$dim_y, 24)
  expect_equal(d$dim_y_1, 2)
  expect_equal(d$dim_y_2, 3)
  expect_equal(d$dim_y_3, 4)
  expect_equal(d$dim_y_12, 6)

  expect_equal(mod$initial(), rep(1, 24))
  expect_equal(mod$deriv(2, mod$initial()), rep(0.2, 24))

  tt <- 0:10
  yy <- mod$run(tt)
  expect_equal(colnames(yy)[[12]], "y[1,3,2]")
  expect_equal(yy[, 1], tt)

  cmp <- deSolve::ode(1, tt, function(t, y, p) list(y * t * 0.1))[, 2]
  expect_equal(
    unname(yy[, -1]),
    matrix(rep(cmp, 24), 11))
})


test_that("rich user arrays", {
  gen <- odin2({
    initial(y[, ]) <- 1
    deriv(y[, ]) <- y[i, j] * r[i, j]
    dim(y) <- c(2, 3)
    r[, ] <- user(min = 0)
    dim(r) <- c(2, 3)
  })

  r <- matrix(runif(6), 2, 3)
  expect_error(gen(r), NA)
  expect_error(gen(-r), "Expected 'r' to be at least 0")
  r[5] <- -1
  expect_error(gen(r), "Expected 'r' to be at least 0")
})


test_that("rich user sized arrays", {
  gen <- odin2({
    initial(y[, ]) <- 1
    deriv(y[, ]) <- y[i, j] * r[i, j]
    dim(y) <- c(2, 3)
    r[, ] <- user(min = 0)
    dim(r) <- user()
  })

  r <- matrix(runif(6), 2, 3)

  expect_error(gen(r), NA)
  expect_error(gen(-r), "Expected 'r' to be at least 0")
  r[5] <- -1
  expect_error(gen(r), "Expected 'r' to be at least 0")
})
