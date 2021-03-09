context("parse: rewriting")

test_that("log", {
  expect_error(odin_parse({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- log()
  }), "Expected 1-2 arguments in log call", class = "odin_error")

  expect_error(odin_parse({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- log(1, 2, 3)
  }), "Expected 1-2 arguments in log call", class = "odin_error")
})


test_that("rewrite arrays drops references to dim_ variables", {
  ir <- odin_parse({
    n <- 2
    m <- 2
    deriv(S[, ]) <- 0
    deriv(I) <- S[n, m]
    dim(S) <- c(n, m)
    initial(S[, ]) <- S0[i, j]
    initial(I) <- 0
    S0[, ] <- user()
    dim(S0) <- c(n, m)
  }, options = odin_options(rewrite_dims = TRUE))
  expect_false(grepl("dim_S_1", ir))
  expect_false(grepl("dim_S", ir))
})
