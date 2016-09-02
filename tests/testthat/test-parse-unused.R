context("parse (unused variables)")

test_that("no unused variables", {
  expect_silent(odin::odin({
    deriv(y) <- 1
    initial(y) <- 0
  }, verbose = TEST_VERBOSE, build = FALSE))
})

test_that("one unused variable", {
  expect_message(odin::odin({
    deriv(y) <- 1
    initial(y) <- 0
    a <- 1
  }, verbose = TEST_VERBOSE, build = FALSE),
  "Unused variable: a")
})

test_that("more than one unused variable", {
  expect_message(odin::odin({
    deriv(y) <- 1
    initial(y) <- 0
    a <- 1
    b <- 2
  }, verbose = TEST_VERBOSE, build = FALSE),
  "Unused variables: a, b")
})

test_that("dependent unused variables", {
  expect_message(odin::odin({
    deriv(y) <- 1
    initial(y) <- 0
    a <- 1
    b <- a * 2
  }, verbose = TEST_VERBOSE, build = FALSE),
  "Unused variables: a, b")
})

test_that("dependent non-unused variables", {
  expect_silent(odin::odin({
    deriv(y) <- b
    initial(y) <- 0
    a <- 1
    b <- a * 2
  }, verbose = TEST_VERBOSE, build = FALSE))
})

test_that("delayed non-unused variables", {
  expect_silent(gen <- odin::odin({
    ylag <- delay(y + a, 10)
    initial(y) <- 0.5
    deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
    a <- 1
  }, verbose=TEST_VERBOSE, build = FALSE))
})
