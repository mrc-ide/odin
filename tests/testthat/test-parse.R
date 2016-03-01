context("parse")

## These are just things I implemented catching; this is far from a
## full set of potential parse errors.  Better error messages will be
## nice for most of these.
test_that("some parse errors", {
  expect_error(odin_parse(text="hello\nfoo"),
               "Every line must contain an assignment")
  expect_error(odin_parse(text="foo ~ bar"),
               "Every line must contain an assignment")

  expect_error(odin_parse(text="1[1:4] <- 1"),
               "array lhs must be a name")
  expect_error(odin_parse(text="x[f(1)] <- 1"),
               "Invalid function in array calculation")
  expect_error(odin_parse(text="x[c(1, 2)] <- 1"),
               "Invalid function in array calculation")

  expect_error(odin_parse(text="x <- 1 + user(2)"),
               "user() must be the only call on the rhs", fixed=TRUE)
  expect_error(odin_parse(text="x <- user(user(2))"),
               "user() call must not use functions", fixed=TRUE)

  expect_error(odin_parse(text="y <- deriv(x)"),
               "Function deriv is disallowed on rhs")

  expect_error(odin_parse(text="dim(x) <- user()"),
               "user() only valid for non-special variables", fixed=TRUE)

  expect_error(odin_parse(text="x[i] <- y[i]"),
               "Special index variable i may not be used on array lhs")

  expect_error(odin_parse(text="x[1,2,3,4] <- 1"),
               "Arrays must have at at most 3 dimensions")

  expect_error(odin_parse(text="x[1:t] <- 1\ndim(x) <- 10"),
               "Array indices may not be time")
  ## TODO: Arguably an error; requires more general solution probably
  ## expect_error(
  ##  odin_parse(text="deriv(A) <- 1\ninitial(A) <- 1\nx[1:A] <- 1; dim(x) <- 1"),
  ##             "Array indices may not be time")
  expect_error(
    odin_parse(text="a[1] <- 1\nb[a] <- 1\ndim(a) <- 1\ndim(b) <- 1"),
    "Array indices may not be arrays")

  expect_error(odin_parse(text="i <- 1"),
               "Reserved name")
  expect_error(odin_parse(text="deriv <- 1"),
               "Reserved name")
  expect_error(odin_parse(text="t <- 1"),
               "Reserved name")
  expect_error(odin_parse(text="dim <- 1"),
               "Reserved name")
  expect_error(odin_parse(text="user <- 1"),
               "Reserved name")

  expect_error(odin_parse(text="deriv_x = 1"),
               "Variable name cannot start with 'deriv_'")
  expect_error(odin_parse(text="dim_x = 1"),
               "Variable name cannot start with 'dim_'")
  expect_error(odin_parse(text="initial_x = 1"),
               "Variable name cannot start with 'initial_'")

  expect_error(odin_parse(text="deriv(y) = 1; initial(x) = 2"),
               "must contain same set of equations")

  expect_error(odin_parse(text="x <- y + z"),
               "Unknown variables y, z")

  expect_error(odin_parse(text="deriv(y) = 1\ninitial(y) = 1\ny <- 1"),
               "variables on lhs must be within deriv")

  expect_error(odin_parse(text="x = 1\nx = 2"),
               "Duplicate entries must all be arrays")
  expect_error(odin_parse(text="x[1] = 1\nx = 2"),
               "Array variables must always assign as arrays")
  expect_error(odin_parse(text="deriv(x[1]) = 1\ninitial(x) = 2"),
               "Array variables must always assign as arrays")

  expect_error(
    odin_parse(text="x[1] <- 1\nx[2,1] <- 2\ndim(x) <- 10"),
    "Array dimensionality is not consistent")
  expect_error(
    odin_parse(text="x[1] <- 1\ny <- x[2,1]\ndim(x) <- 10"),
    "Incorrect dimensionality for x")

  expect_error(odin_parse(text="dim(x[1]) = 1"),
               "must be applied to a name only")
  expect_error(odin_parse(text="dim(x[1,2]) = c(1, 2)"),
               "must be applied to a name only")

  expect_error(odin_parse(text="x <- user(1, 2)"),
               "user() call must have zero or one argument", fixed=TRUE)
  expect_error(odin_parse(text="x <- user(a)"),
               "user() call must not reference variables", fixed=TRUE)

  expect_error(odin_parse(text="y <- x\nx[1] <- 1\ndim(x) <- 10"),
               "Invalid array use on rhs")

  expect_error(odin_parse(text="y[] <- 0\ndim(y) <- f(p)"),
               "Invalid dim() rhs", fixed=TRUE)

  ## TODO: I don't even remember what the issue is here!
  ## expect_error(
  ##   odin_parse(text="dim(x) <- c(10, 10)\nx[1:10,1] <- y[i] * z[j]"),
  ##   ".")

  expect_error(odin_parse(text="a = 1 + delay(1)"),
               "delay() must surround entire rhs", fixed=TRUE)
  expect_error(odin_parse(text="a = delay(1)"),
               "delay() requires exactly two arguments", fixed=TRUE)
  expect_error(odin_parse(text="a = delay(1, 2, 3)"),
               "delay() requires exactly two arguments", fixed=TRUE)
  expect_error(odin_parse(text="a = delay(delay(1, 2), 2)"),
               "delay() may not be nested", fixed=TRUE)
  expect_error(odin_parse(text="a = delay(2, delay(1, 2))"),
               "delay() may not be nested", fixed=TRUE)


})

test_that("RHS array checking", {
  ## This needs expanding as I find more corner cases
  expect_true(check_array_rhs(quote(a + b[1]), c(b=1)))
  expect_false(check_array_rhs(quote(a + b[1]), c(b=2)))
  expect_false(check_array_rhs(quote(a + b[1,2,3]), c(b=2)))
  expect_true(check_array_rhs(quote(a + b[1,2,3]), c(b=3)))
  expect_false(check_array_rhs(quote(a + b[f(1)]), c(b=1)))
  expect_false(check_array_rhs(quote(b), c(b=1)))
  expect_true(check_array_rhs(quote(a), c(b=1)))
  expect_false(check_array_rhs(quote(a[b[1]]), c(a=1, b=1)))
  expect_false(check_array_rhs(quote(a[]), c(a=1)))
})

test_that("lhs array checking", {
  res <- check_array_lhs_index(quote(a + (2:(n-3) - 4) + z))
  expect_true(res)
  expect_equal(attr(res, "value_max"), quote(a + ((n-3) - 4) + z))
  expect_equal(attr(res, "value_min"), quote(a + (2 - 4) + z))

  res <- check_array_lhs_index(quote(a))
  expect_true(res)
  expect_equal(attr(res, "value_max"), quote(a))
  expect_null(attr(res, "value_min"))

  expect_false(check_array_lhs_index(quote(a:b + c:d)))
  expect_false(check_array_lhs_index(quote(-(a:b))))
  expect_false(check_array_lhs_index(quote((a:b):c)))
  expect_false(check_array_lhs_index(quote(c:(a:b))))
  expect_false(check_array_lhs_index(quote((-a))))
})
