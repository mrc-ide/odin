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
               "Invalid functions in array calculation")
  expect_error(odin_parse(text="x[c(1, 2)] <- 1"),
               "Invalid functions in array calculation")
  expect_error(odin_parse(text="y <- deriv(x)"),
               "Function deriv is disallowed on rhs")

  expect_error(odin_parse(text="x[] <- y[i]\ny[1] <- 1"),
               "The empty index is not currently supported")
  expect_error(odin_parse(text="x[,] <- y[i] * z[j]"),
               "The empty index is not currently supported")
  expect_error(odin_parse(text="x[] <- y[i] * z[i]"),
               "The empty index is not currently supported")

  expect_error(odin_parse(text="x[1,2,3,4] <- 1"),
               "Arrays must have at at most 3 dimensions")

  expect_error(odin_parse(text="x[1:t] <- 1"),
               "Array extent is determined by time")
  expect_error(odin_parse(text="deriv(A) <- 1\ninitial(A) <- 1\nx[1:A] <- 1"),
               "Array extent is determined by time")

  expect_error(odin_parse(text="i <- 1"),
               "Reserved name")
  expect_error(odin_parse(text="deriv <- 1"),
               "Reserved name")

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

  expect_error(odin_parse(text="x[1] <- 1\nx[2,1] <- 2"),
               "Array dimensionality is not consistent")

  expect_error(odin_parse(text="deriv_x = 1"),
               "Variable name cannot start with 'deriv_'")

  expect_error(odin_parse(text="x <- user(1, 2)"),
               "user() call must have zero or one argument", fixed=TRUE)
  expect_error(odin_parse(text="x <- user(a)"),
               "user() call must not reference variables", fixed=TRUE)

  ## TODO: this needs way better error messages!
  expect_error(odin_parse(text="y <- x\nx[1] <- 1"),
               "Invalid array use on rhs")
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
