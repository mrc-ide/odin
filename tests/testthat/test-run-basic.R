context("odin: basic")

test_that_odin("trivial model", {
  gen <- odin({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2
  }, options = odin_options(rewrite_constants = FALSE))

  mod <- gen$new()
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
test_that_odin("Time dependent rhs", {
  gen <- odin({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2 * t
  })

  ## This looks like a reasonable rhs but it's going through the
  ## internal storage instead of being transient.
  mod <- gen$new()

  tt <- 0:10
  yy <- mod$run(tt, atol = 1e-8, rtol = 1e-8)
  expect_equal(yy[, 1], tt)
  expect_equal(yy[, 2], 1 + tt^2)

  expect_equal(mod$contents(), list(initial_y = 1))
})


test_that_odin("Time dependent initial conditions", {
  gen <- odin({
    y1 <- cos(t)
    y2 <- y1 * (r + t)
    r <- 1
    deriv(y3) <- y2
    initial(y3) <- y2
  })

  mod <- gen$new()

  f <- function(t) {
    cos(t) * (1 + t)
  }

  expect_equal(mod$initial(0), f(0))
  expect_equal(mod$initial(1), f(1))
  expect_equal(mod$deriv(0, 1), f(0))
  expect_equal(mod$deriv(1, 1), f(1))

  expect_equal(mod$contents()$initial_y3, f(1))
})


## Tests: that we can actually use state variables in a calculation
test_that_odin("use state in derivative calculation", {
  gen <- odin({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- 1
    K <- 100
    r <- 2.0
  })

  mod <- gen$new()
  ## two equilibria:
  expect_equal(mod$deriv(0, 0), 0)
  expect_equal(mod$deriv(0, 100), 0)
  ## off equilibria:
  expect_equal(mod$deriv(0, 1), 2 * 1 * 99 / 100)
})


## Tests: multiple scalar variables can be packed together properly
test_that_odin("multiple variables", {
  gen <- odin({
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
  mod <- gen$new()
  expect_equal(mod$initial(0), c(10, 1, 1))
  expect_equal(mod$deriv(0, mod$initial(0)), c(-90, 269, 22 / 3))
})


## Tests: scalar user variables
test_that_odin("user variables", {
  gen <- odin({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user(1)
    K <- 100
    r <- user()
  })

  expect_error(gen$new())
  expect_error(gen$new(r = NULL),
               "Expected a value for 'r'", fixed = TRUE)
  expect_error(gen$new(r = 1:2),
               "Expected a scalar numeric for 'r'")
  expect_error(gen$new(r = numeric(0)),
               "Expected a scalar numeric for 'r'")

  expect_equal(gen$new(r = pi)$contents()[c("N0", "r")],
               list(N0 = 1, r = pi))
  expect_equal(gen$new(r = pi, N0 = 10)$contents()[c("N0", "r")],
               list(N0 = 10, r = pi))
  expect_equal(gen$new(r = pi, N0 = 10)$initial(0), 10)
  expect_equal(gen$new(r = pi, N0 = 10)$deriv(0, 10),
               pi * 10 * (1 - 10 / 100))

  mod <- gen$new(r = pi, N0 = exp(1))
  mod$set_user()
  expect_equal(mod$contents()$r, pi)
  expect_equal(mod$contents()$N0, exp(1))
})



test_that_odin("simple operations in user variables are allowed", {
  gen <- odin({
    deriv(x) <- 1
    initial(x) <- x0
    x0 <- user(-1 / (2 + 3 * 4))
  })

  mod <- gen$new()
  expect_equal(mod$contents()$x0, -1 / (2 + 3 * 4))
})


## Tests: basic output
##
## This one is about as basic as I can see!
test_that_odin("output", {
  gen <- odin({
    deriv(y) <- 2
    initial(y) <- 1
    output(z) <- t
  })

  tt <- 0:10

  mod <- gen$new()

  expect_equal(mod$deriv(0, 1), structure(2, output = 0))
  expect_equal(mod$deriv(10, 1), structure(2, output = 10))

  yy1 <- mod$run(tt)
  expect_equal(colnames(yy1), c("t", "y", "z"))
  expect_equal(yy1[, "t"], tt)
  expect_equal(yy1[, "y"], seq(1, length.out = length(tt), by = 2))
  expect_equal(yy1[, "z"], tt)

  yy2 <- gen$new(use_dde = TRUE)$run(tt)
  expect_equal(colnames(yy2), c("t", "y", "z"))
  expect_equal(yy2[, "t"], tt)
  expect_equal(yy2[, "y"], seq(1, length.out = length(tt), by = 2))
  expect_equal(yy2[, "z"], tt)
})


## Do some nontrivial calculation in the output
test_that_odin("output", {
  gen <- odin({
    deriv(y) <- 2
    initial(y) <- 1
    output(z) <- a * 2
    a <- t + y
  })

  mod <- gen$new()
  expect_equal(mod$deriv(0, 1), structure(2, output = 2))
  expect_equal(mod$deriv(10, 1), structure(2, output = 22))
})


test_that_odin("copy output", {
  gen <- odin({
    deriv(y) <- 1
    initial(y) <- 1
    z[] <- t
    dim(z) <- 5
    output(z[]) <- TRUE
  })

  mod <- gen$new()
  tt <- 0:10
  y <- mod$run(tt)
  yy <- mod$transform_variables(y)
  expect_equal(yy$y, tt + 1)
  expect_equal(yy$z, matrix(tt, length(tt), 5))
})


test_that_odin("copy output, explicitly", {
  gen <- odin({
    deriv(y) <- 1
    initial(y) <- 1
    z[] <- t
    dim(z) <- 5
    output(z[]) <- z[i]
  })

  mod <- gen$new()
  tt <- 0:10
  y <- mod$run(tt)
  yy <- mod$transform_variables(y)
  expect_equal(yy$y, tt + 1)
  expect_equal(yy$z, matrix(tt, length(tt), 5))
})


## Basic discrete models
test_that_odin("discrete", {
  gen <- odin({
    initial(x) <- 1
    update(x) <- x + 1
  })
  mod <- gen$new()

  expect_equal(mod$initial(0), 1)
  expect_equal(mod$update(0, 1), 2)

  tt <- 0:10
  yy <- mod$run(tt)
  expect_equal(yy, cbind(step = tt, x = tt + 1))
})


test_that_odin("discrete with output", {
  gen <- odin({
    initial(x) <- 1
    update(x) <- x + 1
    output(y) <- x + step
  })
  mod <- gen$new()

  expect_equal(mod$update(2, 3), structure(4, output = 5))
  tt <- 0:10
  yy <- mod$run(tt)
  expect_equal(yy, cbind(step = tt, x = tt + 1, y = 2 * tt + 1))
})


## Fairly minimal array model, though it does mix array and non array
## variables, plus an array support variable.
test_that_odin("array support", {
  gen <- odin({
    initial(x[]) <- 1
    initial(y) <- 2
    deriv(x[]) <- r[i]
    deriv(y) <- n
    r[] <- i
    n <- 3
    dim(r) <- n
    dim(x) <- n
  }, options = odin_options(rewrite_constants = FALSE, rewrite_dims = FALSE))

  mod <- gen$new()

  ## internal data is ok:
  expect_equal(sort_list(mod$contents()),
               sort_list(list(dim_r = 3, dim_x = 3, initial_x = rep(1, 3),
                              initial_y = 2, n = 3, r = 1:3)))
  expect_equal(mod$initial(0), c(2, 1, 1, 1))
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


test_that_odin("multi-line array expression", {
  gen <- odin({
    initial(x) <- 1
    deriv(x) <- 1
    a[1] <- 1
    a[2] <- 1
    a[3:n] <- a[i - 1] + a[i - 2]
    dim(a) <- n
    n <- 10
  })
  expect_equal(gen$new()$contents()$a, c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55))
})


test_that_odin("3d array", {
  gen <- odin({
    initial(y[, , ]) <- 1
    deriv(y[, , ]) <- y[i, j, k] * 0.1
    dim(y) <- c(2, 3, 4)
  }, options = odin_options(rewrite_constants = FALSE, rewrite_dims = FALSE))

  mod <- gen$new()
  d <- mod$contents()
  expect_equal(d$initial_y, array(1, c(2, 3, 4)))
  expect_equal(d$dim_y, 24)
  expect_equal(d$dim_y_1, 2)
  expect_equal(d$dim_y_2, 3)
  expect_equal(d$dim_y_3, 4)
  expect_equal(d$dim_y_12, 6)

  expect_equal(mod$initial(0), rep(1, 24))
  expect_equal(mod$deriv(0, mod$initial(0)), rep(0.1, 24))

  tt <- 0:10
  yy <- mod$run(tt, atol = 1e-8, rtol = 1e-8)
  expect_equal(colnames(yy)[[12]], "y[1,3,2]")
  expect_equal(yy[, 1], tt)
  expect_equal(unname(yy[, -1]), matrix(rep(exp(0.1 * tt), 24), 11))
})


## User provided, constant defined arrays
test_that_odin("user array", {
  gen <- odin({
    initial(x[]) <- 1
    deriv(x[]) <- r[i]
    r[] <- user()
    n <- 3
    dim(r) <- n
    dim(x) <- n
  })

  mod <- gen$new(r = 1:3)
  expect_identical(mod$contents()$r, as.numeric(1:3))
  expect_error(gen$new(r = 1), "Expected length 3 value for 'r'")
})


test_that_odin("user matrix", {
  gen <- odin({
    initial(y[, ]) <- 1
    deriv(y[, ]) <- y[i, j] * r[i, j]
    dim(y) <- c(2, 3)
    dim(r) <- c(2, 3)
    r[, ] <- user()
  })

  r <- matrix(runif(6), 2, 3)
  mod <- gen$new(r = r)
  expect_identical(mod$contents()$r, r)

  ## TODO: this would be nice to tidy up but it's really tricky to
  ## throw these errors the same way in C
  if (odin_target_name() == "r") {
    msg1 <- msg2 <- "Expected a numeric array with dimensions 2 * 3 for 'r'"
  } else {
    msg1 <- "Expected a numeric matrix for 'r'"
    msg2 <- "Incorrect size of dimension 1 of r (expected 2)"
  }

  expect_error(gen$new(r = c(r)), msg1, fixed = TRUE)
  expect_error(gen$new(r = r[2, 2]), msg1, fixed = TRUE)
  expect_error(gen$new(r = array(1, 2:4)), msg1, fixed = TRUE)
  expect_error(gen$new(r = 1), msg1, fixed = TRUE)

  expect_error(gen$new(r = t(r)), msg2, fixed = TRUE)
})


test_that_odin("user array - indirect", {
  gen <- odin({
    initial(x[]) <- 1
    deriv(x[]) <- r[i]
    r[] <- user()
    dim(r) <- n
    dim(x) <- n
    n <- user()
  }, options = odin_options(rewrite_constants = FALSE, rewrite_dims = FALSE))

  mod <- gen$new(n = 3, r = 1:3)
  expect_equal(sort_list(mod$contents()),
               sort_list(list(
                 dim_r = 3,
                 dim_x = 3,
                 initial_x = rep(1, 3),
                 n = 3,
                 r = 1:3)))

  expect_error(gen$new(n = 4, r = 1:3),
               "Expected length 4 value for 'r'")
})


test_that_odin("user array - direct", {
  gen <- odin({
    initial(x[]) <- 1
    deriv(x[]) <- r[i]
    r[] <- user()
    dim(r) <- user()
    dim(x) <- length(r)
  }, options = odin_options(rewrite_constants = FALSE, rewrite_dims = FALSE))

  mod <- gen$new(r = 1:3)
  expect_equal(
    sort_list(mod$contents()),
    sort_list(list(dim_r = 3, dim_x = 3, initial_x = rep(1, 3), r = 1:3)))
  expect_error(gen$new(r = matrix(1, 2, 3)),
               "Expected a numeric vector for 'r'")
  expect_error(gen$new(r = NULL),
               "Expected a value for 'r'")
  expect_silent(mod$set_user(r = NULL))
  expect_equal(mod$contents()$r, 1:3)
})


test_that_odin("user array - direct 3d", {
  gen <- odin({
    initial(y) <- 1
    deriv(y) <- 1
    r[, , ] <- user()
    dim(r) <- user()
  }, options = odin_options(rewrite_constants = FALSE, rewrite_dims = FALSE))

  m <- array(runif(24), 2:4)
  mod <- gen$new(r = m)
  expect_equal(sort_list(mod$contents()),
               sort_list(list(dim_r = 24, dim_r_1 = 2, dim_r_12 = 6,
                              dim_r_2 = 3, dim_r_3 = 4, initial_y = 1,
                              r = m)))

  expect_error(gen$new(r = 1), "Expected a numeric array of rank 3 for 'r'")
  expect_error(gen$new(r = matrix(1)),
               "Expected a numeric array of rank 3 for 'r'")
})


## NOTE: this is the test from test-interpolation.R
test_that_odin("interpolation", {
  gen <- odin({
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
  }, options = odin_options(rewrite_constants = FALSE, rewrite_dims = FALSE))

  tt <- seq(0, 3, length.out = 301)
  tp <- c(0, 1, 2)
  zp <- c(0, 1, 0)
  mod <- gen$new(tp = tp, zp = zp)
  dat <- mod$contents()

  expect_equal(sort(names(dat)),
               sort(c("dim_tp", "dim_zp", "initial_y", "interpolate_pulse",
                      "tp", "zp")))
  pulse <- cinterpolate::interpolation_function(tp, zp, "constant")(tt)
  ## TODO: this can be done for c models too but it requires a bit
  ## more work
  if (odin_target_name() == "r") {
    ## Interpolating function works
    expect_equal(vnapply(tt, dat$interpolate_pulse), pulse)
  }

  yy <- mod$run(tt)
  zz <- ifelse(tt < 1, 0, ifelse(tt > 2, 1, tt - 1))
  expect_equal(yy[, "y"], zz, tolerance = 1e-5)
  expect_equal(yy[, "p"], pulse)
})


test_that_odin("stochastic", {
  gen <- odin({
    initial(x) <- 0
    update(x) <- x + norm_rand()
  })
  mod <- gen$new()
  expect_equal(mod$initial(0), 0)

  set.seed(1)
  x <- rnorm(3)

  set.seed(1)
  y <- replicate(3, mod$update(0, 0))

  expect_identical(x, y)
})


test_that_odin("multiple arrays: constant", {
  gen <- odin({
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

  mod <- gen$new()
  expect_equal(mod$initial(0), rep(1:2, each = 3))
  expect_equal(mod$deriv(0, mod$initial(0)), rep(1:3, 2))
})


test_that_odin("multiple arrays: dynamic", {
  gen <- odin({
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

  mod <- gen$new(n = 4)
  expect_equal(mod$initial(0), rep(1:2, each = 4))
  expect_equal(mod$deriv(0, mod$initial(0)), rep(1:4, 2))
})


test_that_odin("multiple output arrays", {
  gen <- odin({
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
  mod <- gen$new(r = r)

  expect_equal(mod$initial(0), 1:3)
  expect_equal(
    mod$deriv(0, mod$initial(0)),
    structure(1:3 * r, output = c(1:3 / 1:3, r)))

  tt <- seq(0, 10, length.out = 101)
  yy <- mod$run(tt, atol = 1e-8, rtol = 1e-8)
  zz <- mod$transform_variables(yy)

  expect_equal(zz$y, t(1:3 * exp(outer(r, tt))), tolerance = 1e-6)
  expect_equal(zz$r, matrix(r, length(tt), 3, TRUE))
  expect_equal(zz$yr, t(t(zz$y) / (1:3)))
})


test_that_odin("3d array time dependent and variable", {
  gen <- odin({
    initial(y[, , ]) <- 1
    deriv(y[, , ]) <- y[i, j, k] * r[i, j, k]
    dim(y) <- c(2, 3, 4)
    dim(r) <- c(2, 3, 4)
    r[, , ] <- t * 0.1
  }, options = odin_options(rewrite_constants = FALSE, rewrite_dims = FALSE))

  mod <- gen$new()
  d <- mod$contents()
  expect_equal(d$initial_y, array(1, c(2, 3, 4)))
  expect_equal(d$dim_y, 24)
  expect_equal(d$dim_y_1, 2)
  expect_equal(d$dim_y_2, 3)
  expect_equal(d$dim_y_3, 4)
  expect_equal(d$dim_y_12, 6)

  expect_equal(mod$initial(0), rep(1, 24))
  expect_equal(mod$deriv(2, mod$initial(0)), rep(0.2, 24))

  tt <- 0:10
  yy <- mod$run(tt)
  expect_equal(colnames(yy)[[12]], "y[1,3,2]")
  expect_equal(yy[, 1], tt)

  cmp <- deSolve::ode(1, tt, function(t, y, p) list(y * t * 0.1))[, 2]
  expect_equal(
    unname(yy[, -1]),
    matrix(rep(cmp, 24), 11))
})


test_that_odin("rich user arrays", {
  gen <- odin({
    initial(y[, ]) <- 1
    deriv(y[, ]) <- y[i, j] * r[i, j]
    dim(y) <- c(2, 3)
    r[, ] <- user(min = 0)
    dim(r) <- c(2, 3)
  })

  r <- matrix(runif(6), 2, 3)
  expect_error(gen$new(r = r), NA)
  expect_error(gen$new(r = -r), "Expected 'r' to be at least 0")
  r[5] <- -1
  expect_error(gen$new(r = r), "Expected 'r' to be at least 0")
  r[5] <- NA
  expect_error(gen$new(r = r), "'r' must not contain any NA values")
})


test_that_odin("rich user sized arrays", {
  gen <- odin({
    initial(y[, ]) <- 1
    deriv(y[, ]) <- y[i, j] * r[i, j]
    dim(y) <- c(2, 3)
    r[, ] <- user(min = 0)
    dim(r) <- user()
  })

  r <- matrix(runif(6), 2, 3)

  expect_error(gen$new(r = r), NA)
  expect_error(gen$new(r = -r), "Expected 'r' to be at least 0")
  r[5] <- -1
  expect_error(gen$new(r = r), "Expected 'r' to be at least 0")
})


test_that_odin("discrete delays: matrix", {
  gen <- odin({
    initial(y[, ]) <- 1
    update(y[, ]) <- y[i, j] + 1

    initial(z[, ]) <- 1
    update(z[, ]) <- a[i, j]

    a[, ] <- delay(y[i, j], 2)
    dim(y) <- c(2, 3)
    dim(z) <- c(2, 3)
    dim(a) <- c(2, 3)
  }, options = odin_options(rewrite_constants = FALSE, rewrite_dims = FALSE))

  mod <- gen$new()
  tt <- 0:10
  yy <- mod$run(tt)
  zz <- mod$transform_variables(yy)
  expect_equal(zz$z[1:3, , ], array(1, c(3, 2, 3)))
  expect_equal(zz$z[4:11, , ], zz$y[1:8, , ])
})


test_that_odin("multinomial", {
  gen <- odin({
    q[] <- user()
    p[] <- q[i] / sum(q)
    initial(x[]) <- 0
    update(x[]) <- y[i]
    y[] <- rmultinom(5, p)
    dim(p) <- 5
    dim(q) <- 5
    dim(x) <- 5
    dim(y) <- 5
  })

  set.seed(1)
  p <- runif(5)
  mod <- gen$new(q = p)

  set.seed(1)
  y <- mod$update(0, mod$initial(0))
  set.seed(1)
  cmp <- drop(rmultinom(1, 5, p))

  expect_equal(cmp, y)
})


test_that_odin("local scope of loop variables", {
  gen <- odin({
    deriv(x[1, ]) <- 1
    deriv(x[2:n, ]) <- 2

    deriv(y[1, ]) <- 2
    deriv(y[2:n, ]) <- 4

    initial(x[, ]) <- 1
    initial(y[, ]) <- 1

    dim(x) <- c(n, m)
    dim(y) <- c(n, m)
    n <- 4
    m <- 2
  })

  mod <- gen$new()
  y0 <- mod$initial(0)
  y <- mod$transform_variables(mod$deriv(0, y0))

  cmp <- matrix(rep(1:2, c(2, 6)), 4, 2, TRUE)
  expect_equal(y$x, cmp)
  expect_equal(y$y, cmp * 2)
})


test_that_odin("Can set or omit names", {
  gen <- odin({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2
  })
  mod <- gen$new()
  expect_equal(colnames(mod$run(0:10)), c("t", "y"))
  expect_equal(colnames(mod$run(0:10, use_names = FALSE)), NULL)
})


test_that_odin("Can set initial conditions directly in an ode", {
  gen <- odin({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2
  })
  mod <- gen$new()
  y <- mod$run(0:10, 2)
  expect_equal(y[, "y"], seq(2, by = 2, length.out = 11))
})


test_that_odin("Can substitute user variables", {
  gen <- odin({
    n <- user(integer = TRUE)
    m <- user()
    deriv(S[, ]) <- 0
    deriv(I) <- S[n, m]
    dim(S) <- c(n, m)
    initial(S[, ]) <- S0[i, j]
    initial(I) <- 0
    S0[, ] <- user()
    dim(S0) <- c(n, m)
  }, options = odin_options(rewrite_dims = TRUE,
                            rewrite_constants = FALSE,
                            substitutions = list(n = 2, m = 3)))
  expect_equal(nrow(coef(gen)), 1) # only S0 now
  S0 <- matrix(rpois(6, 10), 2, 3)
  mod <- gen$new(S0 = S0)
  dat <- mod$contents()
  expect_equal(dat$n, 2)
  expect_equal(dat$m, 3)
  expect_equal(dat$initial_S, S0)
})


test_that_odin("Can rewrite common dimensions", {
  gen <- odin({
    n <- user(integer = TRUE)
    m <- user()
    deriv(S[, ]) <- 0
    deriv(I) <- S[n, m]
    dim(S) <- c(n, m)
    initial(S[, ]) <- S0[i, j]
    initial(I) <- 0
    S0[, ] <- user()
    dim(S0) <- c(n, m)
  }, options = odin_options(rewrite_constants = TRUE))

  S0 <- matrix(rpois(6, 10), 2, 3)
  mod <- gen$new(S0 = S0, n = 2, m = 3)
  dat <- mod$contents()

  expect_equal(sum(c("dim_S0", "dim_S") %in% names(dat)), 1)
  expect_equal(dat$initial_S, S0)
})
