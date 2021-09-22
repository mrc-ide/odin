context("wrapper")


test_that("force a vector of strings", {
  gen <- odin_js(c("deriv(y) <- 0.5", "initial(y) <- 1"))
  mod <- gen$new()
  y <- mod$run(0:10)[, "y"]
  expect_equal(y, seq(1, by = 0.5, length.out = 11))
})


test_that("force a symbol of code", {
  code <- quote({
    deriv(y) <- 0.5
    initial(y) <- 1
  })
  gen <- odin_js(code)
  mod <- gen$new()
  y <- mod$run(0:10)[, "y"]
  expect_equal(y, seq(1, by = 0.5, length.out = 11))
})


test_that("allow initial conditions", {
  code <- quote({
    deriv(y) <- 0.5
    initial(y) <- 1
  })
  gen <- odin_js(code)
  mod <- gen$new()
  y <- mod$run(0:10, 2)[, "y"]
  expect_equal(y, seq(2, by = 0.5, length.out = 11))
})


test_that("return statistics", {
  code <- quote({
    deriv(y) <- sin(y)
    initial(y) <- 1
  })
  gen <- odin_js(code)
  mod <- gen$new()

  expect_null(attr(mod$run(0:10), "statistics"))
  res <- mod$run(0:10, return_statistics = TRUE)
  statistics <- attr(res, "statistics")
  expect_is(statistics, "integer")
  expect_equal(names(statistics),
               c("n_eval", "n_step", "n_accept", "n_reject"))
  expect_true(all(statistics) >= 0)
})
