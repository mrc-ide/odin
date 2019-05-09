context("parse: delay")

test_that("missing variables in delay", {
  expect_error(odin_parse({
    ylag <- delay(x, 10)
    initial(y) <- 0.5
    deriv(y) <- y + ylag
  }), "Missing variable in delay expression", class = "odin_error")

  expect_error(odin_parse({
    ylag <- delay(x + y, 10)
    initial(y) <- 0.5
    deriv(y) <- y + ylag
  }), "Missing variable in delay expression", class = "odin_error")

  expect_error(odin_parse({
    ylag <- delay(x + z, 10)
    initial(y) <- 0.5
    deriv(y) <- y + ylag
  }), "Missing variables in delay expression", class = "odin_error")
})


test_that("delay call validation", {
  expect_error(odin_parse_(quote(a <- 1 + delay(1))),
               "delay() must be the only call on the rhs",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse_(quote(a <- delay(1))),
               "delay() requires two or three arguments",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse_(quote(a <- delay(1, 2, 3, 4))),
               "delay() requires two or three arguments",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse_(quote(a <- delay(delay(1, 2), 2))),
               "delay() may not be nested",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse_(quote(a <- delay(2, delay(1, 2)))),
               "delay() may not be nested",
               fixed = TRUE, class = "odin_error")

  expect_error(odin_parse_(quote(a <- delay(y + t, 2))),
               "delay() may not refer to time",
               fixed = TRUE, class = "odin_error")
})


test_that("delay check", {
  expect_error(ir_parse_expr(quote(deriv(x) <- delay(y, 1)), NULL, NULL),
               "delay() only valid for non-special variables",
               fixed = TRUE, class = "odin_error")
  expect_error(ir_parse_expr(quote(initial(x) <- delay(y, 1)), NULL, NULL),
               "delay() only valid for non-special variables",
               fixed = TRUE, class = "odin_error")
  expect_error(ir_parse_expr(quote(dim(x) <- delay(y, 1)), NULL, NULL),
               "delay() only valid for non-special variables",
               fixed = TRUE, class = "odin_error")
})


test_that("more parse errors", {
  expect_error(odin_parse({
    x <- y + b
    ylag <- delay(x, 10)
    initial(y) <- 0.5
    deriv(y) <- y + ylag
  }), "Missing variable in delay expression: b (for delay ylag)",
  fixed = TRUE, class = "odin_error")
})


test_that("prevent multiline delay", {
  expect_error(
    odin_parse({
      deriv(a[]) <- i
      initial(a[]) <- (i - 1) / 10
      dim(a) <- 5
      alt[] <- user()
      dim(alt) <- length(a)
      tmp[1] <- delay(a[1], 2, alt[1])
      tmp[2:5] <- delay(a[i], 2, alt[i])
      dim(tmp) <- length(a)
      output(tmp[]) <- TRUE # or tmp[i]
    }),
    "delay() may only be used on a single-line array",
    fixed = TRUE, class = "odin_error")
})
