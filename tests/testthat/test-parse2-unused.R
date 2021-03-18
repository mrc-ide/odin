context("parse: unused variables")

test_that("no unused variables", {
  expect_silent(odin_parse({
    deriv(y) <- 1
    initial(y) <- 0
  }))
})

test_that("one unused variable", {
  expect_message(odin_parse({
    deriv(y) <- 1
    initial(y) <- 0
    a <- 1
  }, options = odin_options(rewrite_constants = FALSE)),
  "Unused equation: a")

  expect_silent(odin_parse({
    deriv(y) <- 1
    initial(y) <- 0
    a <- 1
  }, options = odin_options(rewrite_constants = FALSE,
                            no_check_unused_equations = TRUE)))
})

test_that("more than one unused variable", {
  expect_message(odin_parse({
    deriv(y) <- 1
    initial(y) <- 0
    a <- 1
    b <- 2
  }, options = odin_options(rewrite_constants = FALSE)),
  "Unused equations: a, b")
})

test_that("dependent unused variables", {
  expect_message(odin_parse({
    deriv(y) <- 1
    initial(y) <- 0
    a <- 1
    b <- a * 2
  }, options = odin_options(rewrite_constants = FALSE)),
  "Unused equations: a, b")
})

test_that("dependent non-unused variables", {
  expect_silent(odin_parse({
    deriv(y) <- b
    initial(y) <- 0
    a <- 1
    b <- a * 2
  }, options = odin_options(rewrite_constants = FALSE)))
})

test_that("delayed non-unused variables", {
  expect_silent(odin_parse({
    ylag <- delay(y + a, 10)
    initial(y) <- 0.5
    deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
    a <- 1
  }))
})

test_that("dimension names get cleaned", {
  expect_message(
    odin_parse({
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
    }), "Unused equation: y0")
})


test_that("don't be too noisy", {
  expect_silent(odin_parse({
    initial(y[, , ]) <- 1
    deriv(y[, , ]) <- y[i, j, k] * 0.1
    dim(y) <- c(2, 3, 4)
  }))
})


test_that("Can suppress unused variables with a comment", {
  f <- function(code) {
    odin_parse_(c("initial(x) <- 0", "deriv(x) <- 0", code))
  }
  expect_silent(f("a <- user(1) # ignore.unused"))
  ## If the expression is split over two lines we pick it up:
  expect_silent(f(c("a <-", "  user(1) # ignore.unused")))
  expect_silent(f(c("a <- # ignore.unused", "  user(1)")))

  expect_message(f("a <- user(1) # ignoreUnused"),
                 "Unused equation: a")
  expect_message(f(c("a <- user(1) # ignore.unused",
                     "b <- user(2)")),
                 "Unused equation: b")

  ## Constants are ok
  expect_silent(f("xxx <- 10 # ignore.unused"))

  ## Time varying things should not be removed because they won't be
  ## calculated
  expect_message(
    f("xxx <- 10 * t # ignore.unused"),
    "Unused equation marked as ignored will be dropped: xxx")
})
