context("parse: stochastic")

test_that("disallow stochastic functions in ODEs", {
  ## Here's a stochastic random walk:
  expect_error(odin_parse({
    initial(x) <- 0
    deriv(x) <- x + norm_rand()
  }), "Stochastic functions not allowed in ODE models", class = "odin_error")
})


## This is not allowed directly, though we may allow some via a two
## step process perhaps.
test_that("disallow stochastic functions on array rhs", {
  expect_error(
    odin_parse({
      initial(x[]) <- 1
      dim(x) <- 10
      update(x[runif(1, 10)]) <- 2
    }),
    "Invalid array use on lhs", class = "odin_error")
})
