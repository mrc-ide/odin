context("parse: delay")

test_that("missing variables in delay", {
  expect_error(odin_parse2({
    ylag <- delay(x, 10)
    initial(y) <- 0.5
    deriv(y) <- y + ylag
  }), "Missing variable in delay expression")

  expect_error(odin_parse2({
    ylag <- delay(x + y, 10)
    initial(y) <- 0.5
    deriv(y) <- y + ylag
  }), "Missing variable in delay expression")

  expect_error(odin_parse2({
    ylag <- delay(x + z, 10)
    initial(y) <- 0.5
    deriv(y) <- y + ylag
  }), "Missing variables in delay expression")
})
