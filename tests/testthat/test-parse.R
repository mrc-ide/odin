context("parse")

## These are just things I implemented catching; this is far from a
## full set of potential parse errors.  Better error messages will be
## nice for most of these.
test_that("some parse errors", {
  expect_error(odin_parse("hello\nfoo"),
               "Every line must contain an assignment")
  expect_error(odin_parse(quote(foo ~ bar)),
               "Every line must contain an assignment")

  expect_error(odin_parse(quote(1[1:4] <- 1)),
               "array lhs must be a name")
  expect_error(odin_parse(quote(x[f(1)] <- 1)),
               "Invalid function in array calculation")
  expect_error(odin_parse(quote(x[c(1, 2)] <- 1)),
               "Invalid function in array calculation")

  expect_error(odin_parse(quote(x <- 1 + user(2))),
               "user() must be the only call on the rhs", fixed=TRUE)
  expect_error(odin_parse(quote(x <- user(user(2)))),
               "user() call must not use functions", fixed=TRUE)

  expect_error(odin_parse(quote(y <- deriv(x))),
               "Function deriv is disallowed on rhs")

  expect_error(odin_parse(quote(initial(x) <- user())),
               "user() only valid for non-special variables", fixed=TRUE)
  expect_error(odin_parse(quote(deriv(x) <- user())),
               "user() only valid for non-special variables", fixed=TRUE)
  ## TODO: This gives an unhelpful error message
  ## odin_parse(quote(dim(x) <- user()")

  expect_error(odin_parse(quote(x[i] <- y[i])),
               "Special index variable i may not be used on array lhs")

  expect_error(odin_parse("x[1:t] <- 1\ndim(x) <- 10"),
               "Array indices may not be time")
  ## TODO: Arguably an error; requires more general solution probably
  ## expect_error(
  ##  odin_parse("deriv(A) <- 1\ninitial(A) <- 1\nx[1:A] <- 1; dim(x) <- 1"),
  ##             "Array indices may not be time")
  expect_error(
    odin_parse("a[1] <- 1\nb[a] <- 1\ndim(a) <- 1\ndim(b) <- 1"),
    "Array indices may not be arrays")

  expect_error(odin_parse(quote(i <- 1)),
               "Reserved name")
  expect_error(odin_parse(quote(deriv <- 1)),
               "Reserved name")
  expect_error(odin_parse(quote(t <- 1)),
               "Reserved name")
  expect_error(odin_parse(quote(dim <- 1)),
               "Reserved name")
  expect_error(odin_parse(quote(user <- 1)),
               "Reserved name")

  expect_error(odin_parse(quote(deriv_x <- 1)),
               "Variable name cannot start with 'deriv_'")
  expect_error(odin_parse(quote(dim_x <- 1)),
               "Variable name cannot start with 'dim_'")
  expect_error(odin_parse(quote(initial_x <- 1)),
               "Variable name cannot start with 'initial_'")

  expect_error(odin_parse("deriv(y) = 1; initial(x) = 2"),
               "must contain same set of equations")
  expect_error(odin_parse("update(y) = 1; initial(x) = 2"),
               "must contain same set of equations")

  expect_error(odin_parse("deriv(y) = 1; update(z) = 1; initial(y) = 1; initial(z) = 1;"),
               "Cannot mix deriv() and update()", fixed=TRUE)

  expect_error(odin_parse(quote(x <- y + z)),
               "Unknown variables y, z")

  expect_error(odin_parse("deriv(y) = 1\ninitial(y) = 1\ny <- 1"),
               "variables on lhs must be within deriv")

  expect_error(odin_parse("x = 1\nx = 2"),
               "Duplicate entries must all be array assignments")
  expect_error(odin_parse("x[1] = 1\nx = 2"),
               "Duplicate entries must all be array assignments")
  expect_error(odin_parse("deriv(x[1]) = 1\ninitial(x) = 2"),
               "Missing dim() call", fixed=TRUE)

  expect_error(
    odin_parse("x[1] <- 1\nx[2,1] <- 2\ndim(x) <- 10"),
    "Array dimensionality is not consistent")
  expect_error(
    odin_parse("x[1] <- 1\ny <- x[2,1]\ndim(x) <- 10"),
    "Incorrect dimensionality for 'x'")

  expect_error(odin_parse(quote(dim(x[1]) <- 1)),
               "must be applied to a name only")
  expect_error(odin_parse(quote(dim(x[1,2]) <- c(1, 2))),
               "must be applied to a name only")

  expect_error(odin_parse(quote(x <- user(1, 2))),
               "user() call must have zero or one argument", fixed=TRUE)
  expect_error(odin_parse(quote(x <- user(a))),
               "user() call must not reference variables", fixed=TRUE)

  expect_error(odin_parse("y <- x\nx[1] <- 1\ndim(x) <- 10"),
               "Array 'x' used without array index")

  expect_error(odin_parse("y[] <- 0\ndim(y) <- f(p)"),
               "Invalid dim() rhs", fixed=TRUE)

  ## TODO: I don't even remember what the issue is here!
  ## expect_error(
  ##   odin_parse("dim(x) <- c(10, 10)\nx[1:10,1] <- y[i] * z[j]"),
  ##   ".")

  expect_error(odin_parse(quote(a <- 1 + delay(1))),
               "delay() must be the only call on the rhs", fixed=TRUE)
  expect_error(odin_parse(quote(a <- delay(1))),
               "delay() requires two or three arguments", fixed=TRUE)
  expect_error(odin_parse(quote(a <- delay(1, 2, 3, 4))),
               "delay() requires two or three arguments", fixed=TRUE)
  expect_error(odin_parse(quote(a <- delay(delay(1, 2), 2))),
               "delay() may not be nested", fixed=TRUE)
  expect_error(odin_parse(quote(a <- delay(2, delay(1, 2)))),
               "delay() may not be nested", fixed=TRUE)

  expect_error(odin_parse(quote(a <- delay(y + t, 2))),
               "delay() may not refer to time", fixed=TRUE)

  expect_error(odin_parse(quote(a <- user() + 1)),
               "user() must be the only call on the rhs", fixed=TRUE)
  expect_error(odin_parse(quote(a <- interpolate() + 1)),
               "interpolate() must be the only call on the rhs", fixed=TRUE)

  expect_error(odin_parse(quote(a <- deriv)),
               "Function 'deriv' is disallowed as symbol on rhs")

  expect_error(odin_parse_expr(quote(x <- 1(2)), NULL, NULL),
               "Cannot process statement")
})

test_that("RHS array checking", {
  ## Dummy args:
  line <- 1
  expr <- quote(x)
  ia <- character(0)

  ## This needs expanding as I find more corner cases
  expect_null(odin_parse_arrays_check_rhs(quote(a + b[1]), c(b=1), ia,
                                          line, expr))
  expect_error(odin_parse_arrays_check_rhs(quote(a + b[1]), c(b=2), ia,
                                           line, expr),
               "Incorrect dimensionality for 'b'")
  expect_error(odin_parse_arrays_check_rhs(quote(a + b[1,2,3]), c(b=2), ia,
                                           line, expr),
               "Incorrect dimensionality for 'b'")
  expect_null(odin_parse_arrays_check_rhs(quote(a + b[1,2,3]), c(b=3), ia,
                                          line, expr))
  expect_error(odin_parse_arrays_check_rhs(quote(a + b[f(1)]), c(b=1), ia,
                                           line, expr),
               "Disallowed functions used for b")
  expect_error(odin_parse_arrays_check_rhs(quote(b), c(b=1), ia, line, expr),
               "Array 'b' used without array index")
  expect_null(odin_parse_arrays_check_rhs(quote(a), c(b=1), ia, line, expr))
  expect_error(odin_parse_arrays_check_rhs(quote(a[]), c(a=1), ia, line, expr),
               "Empty array index not allowed on rhs")

  rhs <- odin_parse_expr_rhs_rewrite_sum(quote(sum(a)))
  expect_null(odin_parse_arrays_check_rhs(rhs, c(a=1), ia, line, expr))
  expect_error(odin_parse_arrays_check_rhs(rhs, c(b=1), ia, line, expr),
               "Function 'sum' requires array as first argument")

  rhs <- odin_parse_expr_rhs_rewrite_sum(quote(sum(a[])))
  expect_error(odin_parse_arrays_check_rhs(rhs, c(b=1), ia, line, expr),
               "Function 'sum' requires array as first argument")

  expr <- quote(sum(a[,]))
  rhs <- odin_parse_expr_rhs_rewrite_sum(expr)
  expect_error(odin_parse_arrays_check_rhs(rhs, c(a = 1), ia, line, expr),
               "Incorrect dimensionality for 'a' in 'sum' (expected 1)",
               fixed = TRUE)
  expect_silent(odin_parse_arrays_check_rhs(rhs, c(a = 2), ia, line, expr))
  expect_error(odin_parse_arrays_check_rhs(rhs, c(a = 3), ia, line, expr),
               "Incorrect dimensionality for 'a' in 'sum' (expected 3)",
               fixed = TRUE)
})

test_that("lhs array checking", {
  res <- odin_parse_expr_lhs_check_index(quote(a + (2:(n-3) - 4) + z))
  expect_true(res)
  expect_equal(attr(res, "value_max"), quote(a + ((n-3) - 4) + z))
  expect_equal(attr(res, "value_min"), quote(a + (2 - 4) + z))

  res <- odin_parse_expr_lhs_check_index(quote(a))
  expect_true(res)
  expect_equal(attr(res, "value_max"), quote(a))
  expect_null(attr(res, "value_min"))

  expect_false(odin_parse_expr_lhs_check_index(quote(a:b + c:d)))
  expect_false(odin_parse_expr_lhs_check_index(quote(-(a:b))))
  expect_false(odin_parse_expr_lhs_check_index(quote((a:b):c)))
  expect_false(odin_parse_expr_lhs_check_index(quote(c:(a:b))))
  expect_false(odin_parse_expr_lhs_check_index(quote((-a))))
})

test_that("sum rewriting", {
  ## Dummy args:
  line <- 1
  expr <- quote(x)
  ## This form is not rewritten:
  expect_identical(odin_parse_expr_rhs_rewrite_sum(quote(sum(a)), line, expr),
                   quote(sum(a)))
  expect_error(odin_parse_expr(quote(x <- sum(a, b)), NULL, NULL),
               "Expected 1 argument in sum call")

  ## Start working through some of the more complex cases:
  ## 1d:
  expect_identical(odin_parse_expr_rhs_rewrite_sum(quote(sum(a[]))),
                   quote(odin_sum1(a, 1, length(a))))
  expect_identical(odin_parse_expr_rhs_rewrite_sum(quote(sum(a[b:c]))),
                   quote(odin_sum1(a, b, c)))
  expect_identical(odin_parse_expr_rhs_rewrite_sum(quote(sum(a[4:9]))),
                   quote(odin_sum1(a, 4, 9)))

  ## 2d:
  expect_identical(odin_parse_expr_rhs_rewrite_sum(quote(sum(a[,]))),
                   quote(odin_sum2(a, 1, dim(a, 1), 1, dim(a, 2))))
  expect_identical(odin_parse_expr_rhs_rewrite_sum(quote(sum(a[b:c,]))),
                   quote(odin_sum2(a, b, c, 1, dim(a, 2))))
  expect_identical(odin_parse_expr_rhs_rewrite_sum(quote(sum(a[,d:e]))),
                   quote(odin_sum2(a, 1, dim(a, 1), d, e)))
  expect_identical(odin_parse_expr_rhs_rewrite_sum(quote(sum(a[b:c,d:e]))),
                   quote(odin_sum2(a, b, c, d, e)))

  ## 3d:
  expect_identical(
    odin_parse_expr_rhs_rewrite_sum(quote(sum(a[, , ]))),
    quote(odin_sum3(a, 1, dim(a, 1), 1, dim(a, 2), 1, dim(a, 3))))
  expect_identical(odin_parse_expr_rhs_rewrite_sum(quote(sum(a[b:c, , ]))),
                   quote(odin_sum3(a, b, c, 1, dim(a, 2), 1, dim(a, 3))))
  expect_identical(odin_parse_expr_rhs_rewrite_sum(quote(sum(a[, d:e, ]))),
                   quote(odin_sum3(a, 1, dim(a, 1), d, e, 1, dim(a, 3))))
  expect_identical(odin_parse_expr_rhs_rewrite_sum(quote(sum(a[, , f:g]))),
                   quote(odin_sum3(a, 1, dim(a, 1), 1, dim(a, 2), f, g)))

  expect_identical(odin_parse_expr_rhs_rewrite_sum(
    quote(sum(a[b:c, d:e, f:g]))),
    quote(odin_sum3(a, b, c, d, e, f, g)))

  ## Within a statement:
  expect_identical(
    odin_parse_expr_rhs_rewrite_sum(quote(sum(a[]) + sum(b[]))),
    quote(odin_sum1(a, 1, length(a)) + odin_sum1(b, 1, length(b))))
})

test_that("conditinals need else clause", {
  line <- 1
  expr <- quote(x)
  expect_silent(odin_parse_expr(quote(y <- if (foo) 1 else 2), NULL, NULL))
  expect_error(odin_parse_expr(quote(y <- if (foo) 1), NULL, NULL),
               "All if statements must have an else clause")

  ## Compound:
  expect_silent(odin_parse_expr(
    quote(y <- 1 + (if (foo) 1 else 2) + bar), NULL, NULL))
  expect_error(odin_parse_expr(
    quote(y <- 1 + (if (foo) 1) + bar), NULL, NULL),
    "All if statements must have an else clause")
})

test_that("recursive variables", {
  expect_error(odin_parse_expr(quote(foo <- foo + 1), NULL, NULL),
               "Self referencing expressions not allowed")
})

test_that("array extent and time", {
  expect_error(odin::odin({
    deriv(y[]) <- 1
    initial(y[]) <- 0
    dim(y) <- t
  }), "Array extent may not be time")

  expect_error(odin::odin({
    deriv(y[]) <- 1
    initial(y[]) <- 0
    a <- t
    dim(y) <- a
  }), "Array extent is determined by time")

  expect_error(odin::odin({
    deriv(y[]) <- 1
    initial(y[]) <- 0
    deriv(z) <- 1
    initial(z) <- 0
    dim(y) <- z
  }), "Array extent is determined by time")
})

test_that("lhs checking", {
  expect_error(odin_parse(quote(1 <- 1)),
               "Invalid left hand side")
  expect_error(odin_parse(quote(1 + 1 <- 1)),
               "Unhandled expression + on lhs", fixed=TRUE)
  expect_error(odin_parse(quote(devs(a) <- 1)),
               "Unhandled expression devs on lhs", fixed=TRUE)

  expect_error(odin_parse(quote(deriv(a, b) <- 1)),
               "Invalid length special function on lhs")
  expect_error(odin_parse(quote(initial(a, b) <- 1)),
               "Invalid length special function on lhs")
  expect_error(odin_parse(quote(dim(a, b) <- 1)),
               "Invalid length special function on lhs")

  expect_error(odin_parse(quote(deriv() <- 1)),
               "Invalid length special function on lhs")
  expect_error(odin_parse(quote(initial() <- 1)),
               "Invalid length special function on lhs")
  expect_error(odin_parse(quote(dim() <- 1)),
               "Invalid length special function on lhs")

  expect_error(odin_parse(quote(deriv(deriv(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse(quote(deriv(initial(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse(quote(deriv(dim(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse(quote(initial(deriv(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse(quote(initial(initial(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse(quote(initial(dim(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse(quote(dim(deriv(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse(quote(dim(initial(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse(quote(dim(dim(a)) <- 1)),
               "Invalid nested lhs function usage")
})

test_that("delay time handling", {
  tmp <- odin_parse_expr(quote(a <- delay(b, c + d)), NA_integer_, NULL)
  expect_equal(tmp$rhs$value_time, quote((c + d)))

  tmp <- odin_parse_expr(quote(a <- delay(b, (c + d))), NA_integer_, NULL)
  expect_equal(tmp$rhs$value_time, quote((c + d)))
})

test_that("interpolation", {
  expect_error(odin_parse(quote(x <- interpolate(a, b, c))),
               "Expected a string constant for interpolation type")
  expect_error(odin_parse(quote(x <- interpolate(a, b, 1L))),
               "Expected a string constant for interpolation type")
  expect_error(odin_parse(quote(x <- interpolate(a, b, "lin"))),
               "Invalid interpolation type")

  expect_equal(
    odin_parse_expr(quote(x <- interpolate(a, b)), NULL, NULL)$rhs$value$type,
    "spline")

  expect_error(odin_parse(quote(x <- interpolate(a))),
               "2 or 3 arguments expected")
  expect_error(odin_parse(quote(x <- interpolate(a, b, c, d))),
               "2 or 3 arguments expected")

  expect_error(odin_parse(quote(x <- interpolate(2, x))),
               "interpolation time argument must be a symbol")
  expect_error(odin_parse(quote(x <- interpolate(x, 2))),
               "interpolation target argument must be a symbol")
})

test_that("sums", {
  expect_error(odin_parse_expr(quote(x <- sum(1 + 2)), NULL, NULL),
               "Argument to sum must be a symbol or indexed array")
  expect_error(odin_parse_expr(quote(x <- sum(1)), NULL, NULL),
               "Argument to sum must be a symbol or indexed array")
  expect_error(odin_parse_expr(quote(x <- sum(a, b)), NULL, NULL),
               "Expected 1 argument in sum call, but recieved 2")
  expect_error(odin_parse_expr(quote(x <- sum()), NULL, NULL),
               "Expected 1 argument in sum call, but recieved 0")

  expect_error(odin_parse_expr(quote(x <- sum(a[f(b)])), NULL, NULL),
               "Invalid array use in sum")
  expect_error(odin_parse_expr(quote(x <- sum(a[f(b), c])), NULL, NULL),
               "Invalid array use in sum")
  expect_error(odin_parse_expr(quote(x <- sum(a[f(b), f(c)])), NULL, NULL),
               "Invalid array use in sum")
})


test_that("some dim() pathologies", {
  expect_error(odin_parse_expr(quote(dim(a) <- user(1)), NULL, NULL),
               "Default in user dimension size not handled")
  expect_error(odin_parse_expr(quote(dim(a) <- "foo"), NULL, NULL),
               "expected numeric, symbol, user or c")
  expect_error(odin_parse_expr(quote(dim(a) <- NULL), NULL, NULL),
               "expected numeric, symbol, user or c")
  expect_error(odin_parse_expr(quote(dim(a) <- c(1, "foo")), NULL, NULL),
               "must contain symbols, numbers or lengths")
  expect_error(odin_parse_expr(quote(dim(a) <- c(1, c(2, 3))), NULL, NULL),
               "must contain symbols, numbers or lengths")
  expect_error(odin_parse_expr(quote(dim(a) <- c(1, NULL)), NULL, NULL),
               "must contain symbols, numbers or lengths")
  expect_error(odin_parse_expr(quote(dim(a) <- c(1 + 1, 1)), NULL, NULL),
               "must contain symbols, numbers or lengths")
})

test_that("delay check", {
  expect_error(odin_parse_expr(quote(deriv(x) <- delay(y, 1)), NULL, NULL),
               "delay() only valid for non-special variables", fixed=TRUE)
  expect_error(odin_parse_expr(quote(initial(x) <- delay(y, 1)), NULL, NULL),
               "delay() only valid for non-special variables", fixed=TRUE)
  expect_error(odin_parse_expr(quote(dim(x) <- delay(y, 1)), NULL, NULL),
               "delay() only valid for non-special variables", fixed=TRUE)
})

test_that("array pathologies", {
  expect_error(
    gen <- odin::odin({
      dim(y) <- 2
      y <- 1
    }, verbose=TEST_VERBOSE, build = FALSE)
   , "Array variables must always assign as arrays")

  expect_error(
    gen <- odin::odin({
      dim(y) <- user()
      y <- 1
    }, verbose=TEST_VERBOSE, build = FALSE)
   , "Array variables must always assign as arrays")

  expect_error(
    gen <- odin::odin({
      dim(y) <- 2
      y[1] <- user()
      y[2] <- 1
    }, verbose=TEST_VERBOSE, build = FALSE)
   , "Duplicate entries may not use user")
})

test_that("correct dim() use", {
  expect_error(odin_parse("dim(x) <- 10; a <- length(x, 1)"),
               "Expected 1 argument in length", fixed=TRUE)
  expect_error(odin_parse("dim(x) <- 10; a <- length()"),
               "Expected 1 argument in length", fixed=TRUE)
  expect_error(odin_parse("dim(x) <- 10; a <- length(1)"),
               "argument to length must be a symbol", fixed=TRUE)
  expect_error(odin_parse("x <- 2; a <- length(x)"),
               "argument to length must be an array", fixed=TRUE)
  expect_error(odin_parse("dim(x) <- c(2, 10); a <- length(x)"),
               "argument to length must be a 1-D array", fixed=TRUE)

  expect_error(odin_parse("dim(x) <- 10; a <- dim(x)"),
               "Expected 2 arguments in dim call", fixed=TRUE)
  expect_error(odin_parse("dim(x) <- 10; a <- dim(x, 1, 2)"),
               "Expected 2 arguments in dim call", fixed=TRUE)

  expect_error(odin_parse("dim(x) <- c(10, 2); a <- dim(1, x)"),
               "argument to dim must be a symbol", fixed=TRUE)

  expect_error(odin_parse("x <- 2; a <- dim(x, 1)"),
               "argument to dim must be an array", fixed=TRUE)

  expect_error(odin_parse("dim(x) <- 1; x[] <- 1; a <- dim(x, 1)"),
               "dim() must not be used for 1D arrays", fixed=TRUE)
  expect_error(odin_parse("dim(x) <- c(1, 2); x[,] <- 1; a <- dim(x, 1.4)"),
               "second argument to dim() must be an integer", fixed=TRUE)
  expect_error(odin_parse("dim(x) <- c(1, 2); x[,] <- 1; a <- dim(x, b)"),
               "second argument to dim() must be an integer", fixed=TRUE)

  expect_error(odin_parse("dim(x) <- c(1, 2); x[,] <- 1; a <- dim(x, 3)"),
               "array index out of bounds", fixed=TRUE)

  expect_error(odin_parse("dim(x) <- 10; a <- length(x)"),
               "array variable is never assigned: x")
  expect_error(odin_parse("dim(x) <- 10; dim(y) <- 2"),
               "array variables are never assigned: x, y")
})

test_that("check array rhs", {
  expect_error(
    odin_parse("dim(x) <- 10; x[] <- 1; a <- x[]"),
    "Empty array index not allowed on rhs")

  expect_error(
    odin_parse("dim(x) <- 10; x[] <- 1; a <- x[x]"),
    "Disallowed variables used for x")

  expect_error(
    odin_parse("dim(x) <- 10; y <- 1; a <- x[1] + y[1];"),
    "Unknown array variable y in")
})

## Probably more needed here as there are some special cases...
test_that("cyclic dependency", {
  expect_error(
    odin_parse("a <- b; b <- a"),
    "A cyclic dependency detected")
})

test_that("range operator on RHS", {
  expect_error(
    odin_parse_expr(quote(a <- x[1:2]), NULL, NULL),
    "Range operator ':' may not be used on rhs")
})

## TODO: these (except the first) can all be simplified down to a
## parse_expr call as they no longer need to go all the way through
## odin to get to rewrite to throw the error.
test_that("rewrite errors", {
  expect_error(
    odin({
      deriv(a) <- 1
      initial(a) <- 0
      x <- foo(a)
      output(x) <- x
    }, build = FALSE),
    "Unsupported function: foo")

  expect_error(
    odin({
      deriv(a) <- 1
      initial(a) <- 0
      x <- abs(a, 1)
      output(x) <- x
    }, build = FALSE),
    "Expected 1 argument in abs call")

  expect_error(
    odin({
      deriv(a) <- 1
      initial(a) <- 0
      x <- min(a)
      output(x) <- x
    }, build = FALSE),
    "Expected 2 or more arguments in min call")
})

test_that("Incomplete user array", {
  expect_error(odin::odin({
    initial(x[,]) <- x0[i, j]
    deriv(x[,]) <- 1
    dim(x0) <- user()
    dim(x) <- c(dim(x0, 1), dim(x0, 1))
  }),
  "No array assignment found for x0")
})
