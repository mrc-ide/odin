context("odin-js")

test_that("trivial model", {
  ## TODO: Automate testing if we can test in the absence of V8
  gen <- odin({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2
  }, target = "js")

  mod <- gen$new()
  expect_is(mod, "odin_model")
  expect_equal(mod$initial(0), 1)
  expect_equal(mod$initial(10), 1)
  expect_equal(mod$deriv(0, 0), 2)
  expect_equal(mod$deriv(10, 10), 2)

  tt <- 0:10
  yy <- mod$run(tt)

  expect_equal(colnames(yy), c("t", "y"))
  expect_equal(yy[, "t"], tt)
  expect_equal(yy[, "y"], seq(1, length.out = length(tt), by = 2))

  expect_equal(sort_list(mod$contents()),
               sort_list(list(initial_y = 1, r = 2)))
})


## This tests a few things
##
## 1. can we use time dependent rhs
## 2. can we make transient variables work correctly
## 3. we can construct somewhat nontrivial expressions
##
## This should integrate to a parabola y = 1 + t^2
test_that("Time dependent rhs", {
  gen <- odin({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2 * t
  }, target = "js")

  mod <- gen$new()

  tt <- 0:10
  yy <- mod$run(tt, atol = 1e-8, rtol = 1e-8)
  expect_equal(yy[, 1], tt)
  expect_equal(yy[, 2], 1 + tt^2)

  expect_equal(mod$contents(), list(initial_y = 1))
})


test_that("Time dependent initial conditions", {
  gen <- odin({
    y1 <- cos(t)
    y2 <- y1 * (r + t)
    r <- 1
    deriv(y3) <- y2
    initial(y3) <- y2
  }, target = "js")

  mod <- gen$new()

  f <- function(t) {
    cos(t) * (1 + t)
  }

  expect_equal(mod$initial(0), f(0))
  expect_equal(mod$initial(1), f(1))
  expect_equal(mod$deriv(0, 1), f(0))
  expect_equal(mod$deriv(1, 1), f(1))

  expect_equal(sort_list(mod$contents()),
               sort_list(list(initial_y3 = f(1), r = 1)))
})


test_that("user variables", {
  gen <- odin({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user(1)
    K <- 100
    r <- user()
  }, target = "js")

  expect_error(gen$new(),
               "Expected a value for 'r'")
  ## TODO: Some of these errors are not the same as the other engines
  expect_error(gen$new(user = NULL),
               "Expected a value for 'r'", fixed = TRUE)
  expect_error(gen$new(r = 1:2),
               "Expected a number for 'r'")
  expect_error(gen$new(r = numeric(0)),
               "Expected a number for 'r'")

  expect_equal(sort_list(gen$new(r = pi)$contents()),
               sort_list(list(K = 100, N0 = 1, initial_N = 1, r = pi)))
  expect_equal(sort_list(gen$new(r = pi, N0 = 10)$contents()),
               sort_list(list(K = 100, N0 = 10, initial_N = 10, r = pi)))
  expect_equal(gen$new(r = pi, N0 = 10)$initial(0), 10)
  expect_equal(gen$new(r = pi, N0 = 10)$deriv(0, 10),
               pi * 10 * (1 - 10 / 100))

  mod <- gen$new(r = pi, N0 = exp(1))
  mod$set_user()
  expect_equal(mod$contents()$r, pi)
  expect_equal(mod$contents()$N0, exp(1))
})


test_that("models with output", {
  gen <- odin({
    deriv(y) <- 2
    initial(y) <- 1
    output(z) <- t
  }, target = "js")

  tt <- 0:10

  mod <- gen$new()

  expect_equal(mod$deriv(0, 1), structure(2, output = 0))
  expect_equal(mod$deriv(10, 1), structure(2, output = 10))

  yy1 <- mod$run(tt)
  expect_equal(colnames(yy1), c("t", "y", "z"))
  expect_equal(yy1[, "t"], tt)
  expect_equal(yy1[, "y"], seq(1, length.out = length(tt), by = 2))
  expect_equal(yy1[, "z"], tt)
})


test_that("accept matrices directly if asked nicely", {
  ## We disabled handling matrices like [[a, b, c], [d, e, f]] because
  ## the validation is a bit tedious, and we don't use this for
  ## anything atm.
  testthat::skip("not supported atm")
  gen <- odin({
    deriv(y) <- 1
    initial(y) <- 1
    matrix[, ] <- user()
    dim(matrix) <- user()
  }, target = "js")

  m <- matrix(1:12, c(3, 4))
  mod <- gen$new(matrix = to_json_columnwise(m))
  expect_equal(
    mod$contents()$matrix, m)

  mod <- gen$new(matrix = m)
  expect_equal(
    mod$contents()$matrix, m)
})


test_that("some R functions are not available", {
  expect_error(
    odin({
      deriv(y) <- 1
      initial(y) <- choose(4, 3)
    }, target = "js"),
    "unsupported function 'choose'")
})


test_that("can adjust tolerance in the solver", {
  gen <- odin({
    deriv(y) <- cos(t)
    initial(y) <- 0
  }, target = "js")
  mod <- gen$new()
  tt <- seq(0, 2 * pi, length.out = 101)
  y1 <- mod$run(tt, atol = 1e-3, rtol = 1e-3)
  y2 <- mod$run(tt, atol = 1e-10, rtol = 1e-10)
  expect_true(mean(abs(y1[, 2] - sin(tt))) > 10 * mean(abs(y2[, 2] - sin(tt))))
})


test_that("can adjust max steps", {
  gen <- odin({
    deriv(y) <- cos(t)
    initial(y) <- 0
  }, target = "js")
  mod <- gen$new()
  tt <- seq(0, 2 * pi, length.out = 101)
  expect_error(
    mod$run(tt, step_max_n = 10),
    "Integration failure: too many steps")
})


test_that("can specify min step sizes and allow continuation with them", {
  lorenz <- odin({
    deriv(y1) <- sigma * (y2 - y1)
    deriv(y2) <- R * y1 - y2 - y1 * y3
    deriv(y3) <- -b * y3 + y1 * y2
    initial(y1) <- 10.0
    initial(y2) <- 1.0
    initial(y3) <- 1.0
    sigma <- 10.0
    R     <- 28.0
    b     <-  8.0 / 3.0
  }, target = "js")
  mod <- lorenz$new()
  tt <- seq(0, 1, length.out = 101)
  y1 <- mod$run(tt, return_statistics = TRUE)

  expect_error(
    mod$run(tt, step_size_min = 0.01),
    "Integration failure: step too small")
  y2 <- mod$run(tt, step_size_min = 0.01, step_size_min_allow = TRUE,
                return_statistics = TRUE)
  expect_true(all(attr(y2, "statistics") < attr(y1, "statistics")))
})


test_that("can specify max step sizes", {
  lorenz <- odin({
    deriv(y1) <- sigma * (y2 - y1)
    deriv(y2) <- R * y1 - y2 - y1 * y3
    deriv(y3) <- -b * y3 + y1 * y2
    initial(y1) <- 10.0
    initial(y2) <- 1.0
    initial(y3) <- 1.0
    sigma <- 10.0
    R     <- 28.0
    b     <-  8.0 / 3.0
  }, target = "js")
  mod <- lorenz$new()
  tt <- seq(0, 1, length.out = 101)
  y1 <- mod$run(tt, return_statistics = TRUE)
  y2 <- mod$run(tt, atol = 0.01, rtol = 0.01, step_size_max = 0.01,
                return_statistics = TRUE)
  s1 <- as.list(attr(y1, "statistics"))
  s2 <- as.list(attr(y2, "statistics"))
  expect_gt(s2$n_step, s1$n_step)
  expect_lt(s2$n_reject, s1$n_reject)
})


test_that("Can't include code into js models (yet)", {
  skip_if_not_installed("V8")
  expect_error(odin({
    config(include) <- "user_fns.js"
    z <- squarepulse(t, 1, 2)
    output(z) <- z
    deriv(y) <- z
    initial(y) <- 0
  }, target = "js"),
  "config(include) is not yet supported with JavaScript",
  fixed = TRUE)
})


test_that("Can show generated code", {
  skip_if_not_installed("V8")
  gen <- odin({
    deriv(y) <- 1
    initial(y) <- 1
  }, target = "js")
  code <- gen$public_methods$code()
  expect_type(code, "character")
  expect_equal(code[[1]], "class odin {")
})


test_that("Can show generated code for discrete time models", {
  skip_if_not_installed("V8")
  gen <- odin({
    update(y) <- 1
    initial(y) <- 1
  }, target = "js")
  code <- gen$public_methods$code()
  expect_type(code, "character")
  expect_equal(code[[1]], "class odin {")
})


test_that("Can show versions of js packages", {
  skip_if_not_installed("V8")
  v <- odin_js_versions()
  ## This list may grow over time and that should not fail the tests:
  expect_true(
    all(c("dfoptim", "dopri", "dust", "odinjs", "random") %in% names(v)))
  expect_true(all(vlapply(v, inherits, "numeric_version")))
})


test_that("Can run simple discrete model", {
  gen <- odin({
    update(y) <- y + r
    initial(y) <- 1
    r <- 2
  }, target = "js")

  mod <- gen$new()
  expect_is(mod, "odin_model")
  expect_equal(mod$initial(0), 1)
  expect_equal(mod$initial(10), 1)
  expect_equal(mod$update(0, 0), 2)
  expect_equal(mod$update(10, 10), 12)

  tt <- 0:10
  yy <- mod$run(tt)

  expect_equal(colnames(yy), c("step", "y"))
  expect_equal(yy[, "step"], tt)
  expect_equal(yy[, "y"], seq(1, length.out = length(tt), by = 2))

  expect_equal(sort_list(mod$contents()),
               sort_list(list(initial_y = 1, r = 2)))
})


test_that("can't use output in js discrete time models", {
  expect_error(odin({
    update(y) <- y + r
    initial(y) <- 1
    r <- 2
    output(z) <- y * 2
  }, target = "js"),
  "Using unsupported features: 'has_output'")
})


test_that("can get coefficients from continuous time models", {
  gen <- odin({
    deriv(y) <- r
    initial(y) <- 1
    r <- user(2)
  }, target = "js")
  expected <- data.frame(
    name = "r",
    has_default = TRUE,
    default_value = I(list(2)),
    rank = 0,
    min = -Inf,
    max = Inf,
    integer = FALSE)
  res <- coef(gen)
  expect_equal(res, expected)
  expect_equal(coef(gen$new()), res)
})


test_that("can get coefficients from discrete time models", {
  gen <- odin({
    update(y) <- y + r
    initial(y) <- 1
    r <- user(2)
  }, target = "js")
  expected <- data.frame(
    name = "r",
    has_default = TRUE,
    default_value = I(list(2)),
    rank = 0,
    min = -Inf,
    max = Inf,
    integer = FALSE)
  res <- coef(gen)
  expect_equal(res, expected)
  expect_equal(coef(gen$new()), res)
})


test_that("cast internal arrays to correct dimension", {
  gen <- odin({
    update(y) <- y + sum(r)
    initial(y) <- 1
    r[, ] <- i * j
    dim(r) <- c(3, 4)
  }, target = "js")
  mod <- gen$new()
  res <- mod$contents()
  expect_equal(res$r, outer(1:3, 1:4))
})


test_that("can correctly pull metadata where model has variety of ranks", {
  skip_if_not_installed("V8")
  gen <- odin({
    update(a) <- 1
    update(b[]) <- i
    update(c[, ]) <- i * j
    initial(a) <- 0
    initial(b[]) <- 0
    initial(c[, ]) <- 0
    dim(b) <- 2
    dim(c) <- c(2, 3)
  }, target = "js")
  mod <- gen$new()
  y <- mod$update(0, mod$initial(0))
  expect_equal(y, c(1, 1:2, outer(1:2, 1:3)))
  expect_equal(mod$transform_variables(y),
               list(t = NA_real_,
                    a = 1,
                    b = 1:2,
                    c = outer(1:2, 1:3)))
})
