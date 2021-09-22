context("odin-js")

test_that("trivial model", {
  gen <- odin_js({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2
  })

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
  gen <- odin_js({
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


test_that("Time dependent initial conditions", {
  gen <- odin_js({
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

  expect_equal(sort_list(mod$contents()),
               sort_list(list(initial_y3 = f(1), r = 1)))
})


test_that("user variables", {
  gen <- odin_js({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user(1)
    K <- 100
    r <- user()
  })

  expect_error(gen$new())
  ## TODO: Some of these errors are not the same as the other engines
  expect_error(gen$new(user = NULL),
               "Expected a value for 'r'", fixed = TRUE)
  expect_error(gen$new(r = 1:2),
               "Expected a numeric value for 'r'")
  expect_error(gen$new(r = numeric(0)),
               "Expected a numeric value for 'r'")

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


test_that("accept matrices directly if asked nicely", {
  gen <- odin_js({
    deriv(y) <- 1
    initial(y) <- 1
    matrix[,] <- user()
    dim(matrix) <- user()
  })

  m <- matrix(1:12, c(3, 4))
  mod <- gen$new(matrix = to_json_columnwise(m))
  expect_equal(
    mod$contents()$matrix, m)

  mod <- gen$new(matrix = m)
  expect_equal(
    mod$contents()$matrix, m)
})


test_that("delay models are not supported", {
  expect_error(
    odin_js({
      ylag <- delay(y, 10)
      initial(y) <- 0.5
      deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
    }),
    "Using unsupported features: 'has_delay'")
})


test_that("some R functions are not available", {
  expect_error(
    odin_js({
      deriv(y) <- 1
      initial(y) <- choose(4, 3)
    }),
    "unsupported function 'choose'")
})


test_that("can adjust tolerance in the solver", {
  gen <- odin_js({
    deriv(y) <- cos(t)
    initial(y) <- 0
  })
  mod <- gen$new()
  tt <- seq(0, 2 * pi, length.out = 101)
  y1 <- mod$run(tt, atol = 1e-3, rtol = 1e-3)
  y2 <- mod$run(tt, atol = 1e-10, rtol = 1e-10)
  expect_true(mean(abs(y1[, 2] - sin(tt))) > 10 * mean(abs(y2[, 2] - sin(tt))))
})


test_that("can adjust max steps", {
  gen <- odin_js({
    deriv(y) <- cos(t)
    initial(y) <- 0
  })
  mod <- gen$new()
  tt <- seq(0, 2 * pi, length.out = 101)
  expect_error(
    mod$run(tt, step_max_n = 10),
    "Integration failure: too many steps")
})


test_that("can specify min step sizes and allow continuation with them", {
  lorenz <- odin_js({
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
  lorenz <- odin_js({
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
