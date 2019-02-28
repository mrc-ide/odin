context("parse: unused variables")

test_that("no unused variables", {
  expect_silent(odin_parse2(quote({
    deriv(y) <- 1
    initial(y) <- 0
  })))
})

test_that("one unused variable", {
  expect_message(odin_parse2(quote({
    deriv(y) <- 1
    initial(y) <- 0
    a <- 1
  })), "Unused equation: a")
})

test_that("more than one unused variable", {
  expect_message(odin_parse2(quote({
    deriv(y) <- 1
    initial(y) <- 0
    a <- 1
    b <- 2
  })),
  "Unused equations: a, b")
})

test_that("dependent unused variables", {
  expect_message(odin_parse2(quote({
    deriv(y) <- 1
    initial(y) <- 0
    a <- 1
    b <- a * 2
  })),
  "Unused equations: a, b")
})

test_that("dependent non-unused variables", {
  expect_silent(odin_parse2(quote({
    deriv(y) <- b
    initial(y) <- 0
    a <- 1
    b <- a * 2
  })))
})

test_that("delayed non-unused variables", {
  expect_silent(odin_parse2(quote({
    ylag <- delay(y + a, 10)
    initial(y) <- 0.5
    deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
    a <- 1
  })))
})

test_that("dimension names get cleaned", {
  expect_message(
    odin_parse2(quote({
      deriv(y[]) <- y[i] * r[i]
      initial(y[]) <- i + 1
      y0[] <- i + 1
      dim(y0) <- 3
      dim(y) <- 3
      dim(r) <- 3
      r[] <- user()
      output(yr[]) <- y[i] / (i + 1)
      dim(yr) <- 3
      output(r[]) <- TRUE
      config(base) <- "mod"
    })), "Unused equation: y0")
})
