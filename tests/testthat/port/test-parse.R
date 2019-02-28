context("parse")

## These are just things I implemented catching; this is far from a
## full set of potential parse errors.  Better error messages will be
## nice for most of these.
test_that("every line contains an assignment", {
  expect_error(odin_parse2("hello\nfoo"),
               "Every line must contain an assignment")
  expect_error(odin_parse2(quote(foo ~ bar)),
               "Every line must contain an assignment")
})


## These all happen early enough that we can ignore problems with the
## initial/deriv bit being missing:
test_that("expression parsing", {
  expect_error(odin_parse2(quote(1[1:4] <- 1)),
               "array lhs must be a name")
  expect_error(odin_parse2(quote(x[f(1)] <- 1)),
               "Invalid function in array calculation")
  expect_error(odin_parse2(quote(x[c(1, 2)] <- 1)),
               "Invalid function in array calculation")

  expect_error(odin_parse2(quote(x <- 1 + user(2))),
               "user() must be the only call on the rhs", fixed = TRUE)

  expect_error(odin_parse2(quote(y <- deriv(x))),
               "Function deriv is disallowed on rhs")

  expect_error(odin_parse2(quote(initial(x) <- user())),
               "user() only valid for non-special variables", fixed = TRUE)
  expect_error(odin_parse2(quote(deriv(x) <- user())),
               "user() only valid for non-special variables", fixed = TRUE)
  expect_error(odin_parse2(quote(x[i] <- y[i])),
               "Special index variable i may not be used on array lhs")
})


test_that("parse array indices", {
  expect_error(odin_parse2(
    "initial(y) <- 1; deriv(y) <- 1; x[1:t] <- 1\ndim(x) <- 10"),
    "Array indices may not be time")

  ## TODO: Arguably an error; requires more general solution probably
  ## expect_error(
  ##  odin_parse2("deriv(A) <- 1\ninitial(A) <- 1\nx[1:A] <- 1; dim(x) <- 1"),
  ##             "Array indices may not be time")

  expect_error(
    odin_parse2(ex("x[1] <- 1\ny[x] <- 1\ndim(x) <- 1\ndim(y) <- 1")),
    "Array indices may not be arrays")
})


test_that("reserved names", {
  expect_error(odin_parse2(quote(i <- 1)),
               "Reserved name")
  expect_error(odin_parse2(quote(deriv <- 1)),
               "Reserved name")
  expect_error(odin_parse2(quote(t <- 1)),
               "Reserved name")
  expect_error(odin_parse2(quote(dim <- 1)),
               "Reserved name")
  expect_error(odin_parse2(quote(user <- 1)),
               "Reserved name")

  expect_error(odin_parse2(quote(deriv_x <- 1)),
               "Variable name cannot start with 'deriv_'")
  expect_error(odin_parse2(quote(dim_x <- 1)),
               "Variable name cannot start with 'dim_'")
  expect_error(odin_parse2(quote(initial_x <- 1)),
               "Variable name cannot start with 'initial_'")
})


test_that("compatible rhs", {
  expect_error(odin_parse2("deriv(y) = 1; initial(x) = 2"),
               "must contain same set of equations")
  expect_error(odin_parse2("update(y) = 1; initial(x) = 2"),
               "must contain same set of equations")

  expect_error(odin_parse2(
    "deriv(y) = 1; update(z) = 1; initial(y) = 1; initial(z) = 1;"),
    "Cannot mix deriv() and update()", fixed = TRUE)
})


test_that("unknown variables", {
  expect_error(odin_parse2(
    ex("x <- y + z")),
    "Unknown variables y, z")
})


test_that("variables cannot be assigned to", {
  expect_error(odin_parse2("deriv(y) = 1\ninitial(y) = 1\ny <- 1"),
               "variables on lhs must be within deriv")
})


test_that("duplicate non-array entries", {
  expect_error(
    odin_parse2(ex("x = 1\nx = 2")),
    "Duplicate entries must all be array assignments")
  expect_error(
    odin_parse2(ex("x[1] = 1\nx = 2")),
    "Duplicate entries must all be array assignments")
})


test_that("missing dim", {
  expect_error(
    odin_parse2("deriv(x[1]) = 1\ninitial(x) = 2"),
    "Missing dim() call", fixed = TRUE)
})


test_that("array dimensionality must be consistent", {
  expect_error(
    odin_parse2(ex("x[1] <- 1\nx[2,1] <- 2\ndim(x) <- 10")),
    "Array dimensionality is not consistent")
  expect_error(
    odin_parse2(ex("x[1] <- 1\ny <- x[2,1]\ndim(x) <- 10")),
    "Incorrect dimensionality for 'x'")
})


test_that("dim lhs must be simple", {
  expect_error(odin_parse2(quote(dim(x[1]) <- 1)),
               "must be applied to a name only")
  expect_error(odin_parse2(quote(dim(x[1,2]) <- c(1, 2))),
               "must be applied to a name only")
})


test_that("user usage", {
  expect_error(odin_parse2("x <- user(a, b)"),
               "Only first argument to user() may be unnamed", fixed = TRUE)
  expect_error(odin_parse2("x <- user(a, c = 1)"),
               "Unknown argument to user(): 'c'", fixed = TRUE)
  expect_error(odin_parse2(quote(x <- user(user(2)))),
               "user() call must not use functions", fixed = TRUE)
  expect_error(odin_parse2(quote(x <- user(a))),
               "user() call must not reference variables", fixed = TRUE)
})


test_that("array usage", {
  expect_error(odin_parse2(ex("y <- x\nx[1] <- 1\ndim(x) <- 10")),
               "Array 'x' used without array index")
})


test_that("dim rhs must be simple", {
  expect_error(odin_parse2(ex("y[] <- 0\ndim(y) <- f(p)")),
               "Invalid dim() rhs", fixed = TRUE)
})


test_that("delay call validation", {
  expect_error(odin_parse2(quote(a <- 1 + delay(1))),
               "delay() must be the only call on the rhs", fixed = TRUE)
  expect_error(odin_parse2(quote(a <- delay(1))),
               "delay() requires two or three arguments", fixed = TRUE)
  expect_error(odin_parse2(quote(a <- delay(1, 2, 3, 4))),
               "delay() requires two or three arguments", fixed = TRUE)
  expect_error(odin_parse2(quote(a <- delay(delay(1, 2), 2))),
               "delay() may not be nested", fixed = TRUE)
  expect_error(odin_parse2(quote(a <- delay(2, delay(1, 2)))),
               "delay() may not be nested", fixed = TRUE)

  expect_error(odin_parse2(quote(a <- delay(y + t, 2))),
               "delay() may not refer to time", fixed = TRUE)
})


test_that("user call", {
  expect_error(odin_parse2(quote(a <- user() + 1)),
               "user() must be the only call on the rhs", fixed = TRUE)
})


test_that("interpolate call", {
  expect_error(odin_parse2(quote(a <- interpolate() + 1)),
               "interpolate() must be the only call on the rhs", fixed = TRUE)
})


test_that("deriv can't be used as rhs symbol", {
  expect_error(odin_parse2(ex("a <- deriv")),
               "Function 'deriv' is disallowed as symbol on rhs")
})


test_that("unclassifiable statement", {
  expect_error(odin_parse2(quote(x <- 1(2))),
               "Cannot process statement")
})


test_that("RHS array checking", {
  ## Dummy args:
  eq <- list(source = 1)
  expr <- quote(x)
  ia <- character(0)
  source <- "x"

  ## This needs expanding as I find more corner cases
  ##
  ## TODO: I would prefer this to go all the way from odin_parse2
  expect_null(ir_parse_arrays_check_rhs(quote(a + b[1]), c(b = 1), ia,
                                        eq, source))
  expect_error(ir_parse_arrays_check_rhs(quote(a + b[1]), c(b = 2), ia,
                                         eq, source),
               "Incorrect dimensionality for 'b'")
  expect_error(ir_parse_arrays_check_rhs(quote(a + b[1,2,3]), c(b = 2), ia,
                                         eq, source),
               "Incorrect dimensionality for 'b'")
  expect_null(ir_parse_arrays_check_rhs(quote(a + b[1,2,3]), c(b = 3), ia,
                                        eq, source))
  expect_error(ir_parse_arrays_check_rhs(quote(a + b[f(1)]), c(b = 1), ia,
                                         eq, source),
               "Disallowed functions used for b")
  expect_error(ir_parse_arrays_check_rhs(quote(b), c(b = 1), ia,
                                         eq, source),
               "Array 'b' used without array index")
  expect_null(ir_parse_arrays_check_rhs(quote(a), c(b = 1), ia,
                                        eq, source))
  expect_error(ir_parse_arrays_check_rhs(quote(a[]), c(a = 1), ia,
                                         eq, source),
               "Empty array index not allowed on rhs")

  rhs <- ir_parse_expr_rhs_expression_sum(quote(sum(a)))
  expect_null(ir_parse_arrays_check_rhs(rhs, c(a = 1), ia, eq, source))
  expect_error(ir_parse_arrays_check_rhs(rhs, c(b = 1), ia, eq, source),
               "Function 'sum' requires array as argument 1")

  rhs <- ir_parse_expr_rhs_expression_sum(quote(sum(a[])))
  expect_error(ir_parse_arrays_check_rhs(rhs, c(b = 1), ia, eq, source),
               "Function 'sum' requires array as argument 1")

  expr <- quote(sum(a[,]))
  rhs <- ir_parse_expr_rhs_expression_sum(expr)
  expect_error(ir_parse_arrays_check_rhs(rhs, c(a = 1), ia, eq, source),
               "Incorrect dimensionality for 'a' in 'sum' (expected 1)",
               fixed = TRUE)
  expect_silent(ir_parse_arrays_check_rhs(rhs, c(a = 2), ia, eq, source))
  expect_error(ir_parse_arrays_check_rhs(rhs, c(a = 3), ia, eq, source),
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
  ## This form is not rewritten:
  expect_identical(ir_parse_expr_rhs_expression_sum(quote(sum(a))),
                   quote(sum(a)))
  expect_error(odin_parse2("x <- sum(a, b)"),
               "Expected 1 argument in sum call")

  ## Start working through some of the more complex cases:
  ## 1d:
  expect_identical(ir_parse_expr_rhs_expression_sum(quote(sum(a[]))),
                   quote(odin_sum(a, 1, length(a))))
  expect_identical(ir_parse_expr_rhs_expression_sum(quote(sum(a[b:c]))),
                   quote(odin_sum(a, b, c)))
  expect_identical(ir_parse_expr_rhs_expression_sum(quote(sum(a[4:9]))),
                   quote(odin_sum(a, 4, 9)))

  ## 2d:
  expect_identical(ir_parse_expr_rhs_expression_sum(quote(sum(a[,]))),
                   quote(odin_sum(a, 1, dim(a, 1), 1, dim(a, 2))))
  expect_identical(ir_parse_expr_rhs_expression_sum(quote(sum(a[b:c,]))),
                   quote(odin_sum(a, b, c, 1, dim(a, 2))))
  expect_identical(ir_parse_expr_rhs_expression_sum(quote(sum(a[,d:e]))),
                   quote(odin_sum(a, 1, dim(a, 1), d, e)))
  expect_identical(ir_parse_expr_rhs_expression_sum(quote(sum(a[b:c,d:e]))),
                   quote(odin_sum(a, b, c, d, e)))

  ## 3d:
  expect_identical(
    ir_parse_expr_rhs_expression_sum(quote(sum(a[, , ]))),
    quote(odin_sum(a, 1, dim(a, 1), 1, dim(a, 2), 1, dim(a, 3))))
  expect_identical(ir_parse_expr_rhs_expression_sum(quote(sum(a[b:c, , ]))),
                   quote(odin_sum(a, b, c, 1, dim(a, 2), 1, dim(a, 3))))
  expect_identical(ir_parse_expr_rhs_expression_sum(quote(sum(a[, d:e, ]))),
                   quote(odin_sum(a, 1, dim(a, 1), d, e, 1, dim(a, 3))))
  expect_identical(ir_parse_expr_rhs_expression_sum(quote(sum(a[, , f:g]))),
                   quote(odin_sum(a, 1, dim(a, 1), 1, dim(a, 2), f, g)))

  expect_identical(ir_parse_expr_rhs_expression_sum(
    quote(sum(a[b:c, d:e, f:g]))),
    quote(odin_sum(a, b, c, d, e, f, g)))

  ## Within a statement:
  expect_identical(
    ir_parse_expr_rhs_expression_sum(quote(sum(a[]) + sum(b[]))),
    quote(odin_sum(a, 1, length(a)) + odin_sum(b, 1, length(b))))
})

test_that("conditinals need else clause", {
  expect_error(
    odin_parse2("y <- if (foo) 1"),
    "All if statements must have an else clause")
  expect_error(
    odin_parse2("y <- 1 + (if (foo) 1) + bar"),
    "All if statements must have an else clause")
})

test_that("recursive variables", {
  expect_error(odin_parse2(quote(foo <- foo + 1)),
               "Self referencing expressions not allowed")
})

test_that("array extent and time", {
  expect_error(odin_parse2(quote({
    deriv(y[]) <- 1
    initial(y[]) <- 0
    dim(y) <- t
  })), "Array extent is determined by time")

  expect_error(odin_parse2(quote({
    deriv(y[]) <- 1
    initial(y[]) <- 0
    a <- t
    dim(y) <- a
  })), "Array extent is determined by time")

  expect_error(odin_parse2(quote({
    deriv(y[]) <- 1
    initial(y[]) <- 0
    deriv(z) <- 1
    initial(z) <- 0
    dim(y) <- z
  })), "Array extent is determined by time")
})

test_that("lhs checking", {
  expect_error(odin_parse2(quote(1 <- 1)),
               "Invalid left hand side")

  expect_error(odin_parse2(quote(1 + 1 <- 1)),
               "Unhandled expression + on lhs", fixed = TRUE)
  expect_error(odin_parse2(quote(devs(a) <- 1)),
               "Unhandled expression devs on lhs", fixed = TRUE)

  expect_error(odin_parse2(quote(deriv(a, b) <- 1)),
               "Invalid length special function on lhs")
  expect_error(odin_parse2(quote(initial(a, b) <- 1)),
               "Invalid length special function on lhs")
  expect_error(odin_parse2(quote(dim(a, b) <- 1)),
               "Invalid length special function on lhs")

  expect_error(odin_parse2(quote(deriv() <- 1)),
               "Invalid length special function on lhs")
  expect_error(odin_parse2(quote(initial() <- 1)),
               "Invalid length special function on lhs")
  expect_error(odin_parse2(quote(dim() <- 1)),
               "Invalid length special function on lhs")

  expect_error(odin_parse2(quote(deriv(deriv(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse2(quote(deriv(initial(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse2(quote(deriv(dim(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse2(quote(initial(deriv(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse2(quote(initial(initial(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse2(quote(initial(dim(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse2(quote(dim(deriv(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse2(quote(dim(initial(a)) <- 1)),
               "Invalid nested lhs function usage")
  expect_error(odin_parse2(quote(dim(dim(a)) <- 1)),
               "Invalid nested lhs function usage")
})

test_that("delay time handling", {
  tmp <- odin_parse_expr(quote(a <- delay(b, c + d)), NA_integer_, NULL)
  expect_equal(tmp$rhs$value_time, quote((c + d)))

  tmp <- odin_parse_expr(quote(a <- delay(b, (c + d))), NA_integer_, NULL)
  expect_equal(tmp$rhs$value_time, quote((c + d)))
})

test_that("interpolation", {
  expect_error(odin_parse2(quote(x <- interpolate(a, b, c))),
               "Expected a string constant for interpolation type")
  expect_error(odin_parse2(quote(x <- interpolate(a, b, 1L))),
               "Expected a string constant for interpolation type")
  expect_error(odin_parse2(quote(x <- interpolate(a, b, "lin"))),
               "Invalid interpolation type")

  expect_equal(
    odin_parse_expr(quote(x <- interpolate(a, b)), NULL, NULL)$rhs$value$type,
    "spline")

  expect_error(odin_parse2(quote(x <- interpolate(a))),
               "interpolate() requires two or three arguments", fixed = TRUE)
  expect_error(odin_parse2(quote(x <- interpolate(a, b, c, d))),
               "interpolate() requires two or three arguments", fixed = TRUE)

  expect_error(odin_parse2(quote(x <- interpolate(2, x))),
               "interpolation time argument must be a symbol")
  expect_error(odin_parse2(quote(x <- interpolate(x, 2))),
               "interpolation target argument must be a symbol")
})

test_that("sums", {
  expect_error(odin_parse2(quote(x <- sum(1 + 2))),
               "Argument to sum must be a symbol or indexed array")
  expect_error(odin_parse2(quote(x <- sum(1))),
               "Argument to sum must be a symbol or indexed array")
  expect_error(odin_parse2(quote(x <- sum(a, b))),
               "Expected 1 argument in sum call, but recieved 2")
  expect_error(odin_parse2(quote(x <- sum())),
               "Expected 1 argument in sum call, but recieved 0")

  expect_error(odin_parse2(quote(x <- sum(a[f(b)]))),
               "Invalid array use in sum")
  expect_error(odin_parse2(quote(x <- sum(a[f(b), c]))),
               "Invalid array use in sum")
  expect_error(odin_parse2(quote(x <- sum(a[f(b), f(c)]))),
               "Invalid array use in sum")
})


test_that("some dim() pathologies", {
  expect_error(odin_parse2(ex("a[] <- user(); dim(a) <- user(1)")),
               "Default in user dimension size not handled")
  expect_error(odin_parse2(ex("dim(a) <- 'foo'")),
               "expected numeric, symbol, user or c")
  expect_error(odin_parse2(ex("dim(a) <- NULL")),
               "expected numeric, symbol, user or c")
  expect_error(odin_parse2(ex("dim(a) <- c(1, 'foo')")),
               "must contain symbols, numbers or lengths")
  expect_error(odin_parse2(ex("dim(a) <- c(1, c(2, 3))")),
               "must contain symbols, numbers or lengths")
  expect_error(odin_parse2(ex("dim(a) <- c(1, NULL)")),
               "must contain symbols, numbers or lengths")
  expect_error(odin_parse2(ex("dim(a) <- c(1 + 1, 1)")),
               "must contain symbols, numbers or lengths")
})

test_that("delay check", {
  expect_error(ir_parse_expr(quote(deriv(x) <- delay(y, 1)), NULL, NULL),
               "delay() only valid for non-special variables", fixed = TRUE)
  expect_error(ir_parse_expr(quote(initial(x) <- delay(y, 1)), NULL, NULL),
               "delay() only valid for non-special variables", fixed = TRUE)
  expect_error(ir_parse_expr(quote(dim(x) <- delay(y, 1)), NULL, NULL),
               "delay() only valid for non-special variables", fixed = TRUE)
})

test_that("array pathologies", {
  expect_error(
    odin_parse2(ex("dim(y) <- 2; y <- 1")),
    "Array variables must always assign as arrays")
  expect_error(
    odin_parse2(ex("dim(y) <- user(); y <- 1")),
    "Array variables must always assign as arrays")
  expect_error(
    odin_parse2(ex("dim(y) <- 2; y[1] <- user(); y[2] <- 1")),
    "user() may only be used on a single-line array", fixed = TRUE)
})

test_that("correct dim() use", {
  expect_error(odin_parse2("dim(x) <- 10; a <- length(x, 1)"),
               "Expected 1 argument in length", fixed = TRUE)
  expect_error(odin_parse2("dim(x) <- 10; a <- length()"),
               "Expected 1 argument in length", fixed = TRUE)

  expect_error(odin_parse2(ex("a <- length(1)")),
               "argument to length must be a symbol", fixed = TRUE)
  expect_error(odin_parse2(ex("x <- 2; a <- length(x)")),
               "argument to length must be an array", fixed = TRUE)
  expect_error(odin_parse2(ex(
    "dim(x) <- c(2, 10); x[, ] <- 1; a <- length(x)")),
    "argument to length must be a 1-D array", fixed = TRUE)

  expect_error(odin_parse2("dim(x) <- 10; a <- dim(x)"),
               "Expected 2 arguments in dim call", fixed = TRUE)
  expect_error(odin_parse2("dim(x) <- 10; a <- dim(x, 1, 2)"),
               "Expected 2 arguments in dim call", fixed = TRUE)

  expect_error(odin_parse2(ex("a <- dim(1, x)")),
               "first argument to dim must be a symbol", fixed = TRUE)

  expect_error(odin_parse2(ex("x <- 2; a <- dim(x, 1)")),
               "first argument to dim must be an array", fixed = TRUE)

  expect_error(odin_parse2(ex(
    "dim(x) <- 1; x[] <- 1; a <- dim(x, 1)")),
    "dim() must not be used for 1D arrays", fixed = TRUE)
  expect_error(odin_parse2(ex(
    "dim(x) <- c(1, 2); x[,] <- 1; a <- dim(x, 1.4)")),
    "second argument to dim() must be an integer", fixed = TRUE)
  expect_error(odin_parse2(ex(
    "dim(x) <- c(1, 2); x[,] <- 1; a <- dim(x, b)")),
    "second argument to dim() must be an integer", fixed = TRUE)

  expect_error(odin_parse2(ex(
    "dim(x) <- c(1, 2); x[,] <- 1; a <- dim(x, 3)")),
    "array index out of bounds", fixed = TRUE)

  expect_error(odin_parse(ex("dim(x) <- 10; b <- length(x)")),
               "array variable is never assigned: x")
})

test_that("check array rhs", {
  expect_error(
    odin_parse2(ex("dim(x) <- 10; x[] <- 1; b <- x[]")),
    "Empty array index not allowed on rhs")

  expect_error(
    odin_parse2(ex("dim(x) <- 10; x[] <- 1; a <- x[x]")),
    "Disallowed variables used for x")

  expect_error(
    odin_parse2(ex("dim(x) <- 10; x[] <- 1; y <- 1; a <- x[1] + y[1];")),
    "Unknown array variable y in")
})

## Probably more needed here as there are some special cases...
test_that("cyclic dependency", {
  expect_error(
    odin_parse2(ex("x <- y; y <- x")),
    "A cyclic dependency detected")
})

test_that("range operator on RHS", {
  expect_error(odin_parse2("y <- x[1:2]"),
               "Range operator ':' may not be used on rhs")
})

test_that("rewrite errors", {
  expect_error(odin_parse2(ex("a <- 1; x <- foo(a)")),
               "Unsupported function: foo")
  expect_error(odin_parse2(ex("a <- 1; x <- foo(a); y <- bar(a)")),
               "Unsupported functions: foo, bar")
  expect_error(odin_parse2(ex("a <- 1; x <- abs(a, 1)")),
               "Expected 1 argument in abs call")
  expect_error(odin_parse2(ex("a <- 1; x <- min(a)")),
               "Expected 2 or more arguments in min call")
})

test_that("Incomplete user array", {
  expect_error(odin_parse2(quote({
    initial(x[,]) <- x0[i, j]
    deriv(x[,]) <- 1
    dim(x0) <- user()
    dim(x) <- c(dim(x0, 1), dim(x0, 1))
  })),
  "No array assignment found for x0")

  expect_error(
    odin_parse2(ex("dim(x) <- user()")),
    "No array assignment found for x")
})
