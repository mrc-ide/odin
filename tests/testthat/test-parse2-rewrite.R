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
