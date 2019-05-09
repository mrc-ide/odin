context("parse: general")

## These are just things I implemented catching; this is far from a
## full set of potential parse errors.  Better error messages will be
## nice for most of these.
test_that("every line contains an assignment", {
  expect_error(odin_parse_("hello\nfoo"),
               "Every line must contain an assignment",
               class = "odin_error")
  expect_error(odin_parse_(quote(foo ~ bar)),
               "Every line must contain an assignment",
               class = "odin_error")
})


## These all happen early enough that we can ignore problems with the
## initial/deriv bit being missing:
test_that("expression parsing", {
  expect_error(odin_parse_(quote(1[1:4] <- 1)),
               "array lhs must be a name",
               class = "odin_error")
  expect_error(odin_parse_(quote(x[f(1)] <- 1)),
               "Invalid function in array calculation",
               class = "odin_error")
  expect_error(odin_parse_(quote(x[c(1, 2)] <- 1)),
               "Invalid function in array calculation",
               class = "odin_error")

  expect_error(odin_parse_(quote(x <- 1 + user(2))),
               "user() must be the only call on the rhs",
               fixed = TRUE, class = "odin_error")

  expect_error(odin_parse_(quote(y <- deriv(x))),
               "Function deriv is disallowed on rhs", class = "odin_error")

  expect_error(odin_parse_(quote(initial(x) <- user())),
               "user() only valid for non-special variables",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse_(quote(deriv(x) <- user())),
               "user() only valid for non-special variables",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse_(quote(x[i] <- y[i])),
               "Special index variable i may not be used on array lhs",
               class = "odin_error")
})


test_that("parse array indices", {
  expect_error(odin_parse(
    "initial(y) <- 1; deriv(y) <- 1; x[1:t] <- 1\ndim(x) <- 10"),
    "Array indices may not be time",
    class = "odin_error")

  ## TODO: Arguably an error; requires more general solution probably
  ## expect_error(
  ##  odin_parse("deriv(A) <- 1\ninitial(A) <- 1\nx[1:A] <- 1; dim(x) <- 1"),
  ##             "Array indices may not be time")

  expect_error(
    odin_parse_(ex("x[1] <- 1\ny[x] <- 1\ndim(x) <- 1\ndim(y) <- 1")),
    "Array indices may not be arrays",
    class = "odin_error")
})


test_that("reserved names", {
  expect_error(odin_parse_(quote(i <- 1)),
               "Reserved name", class = "odin_error")
  expect_error(odin_parse_(quote(deriv <- 1)),
               "Reserved name", class = "odin_error")
  expect_error(odin_parse_(quote(t <- 1)),
               "Reserved name", class = "odin_error")
  expect_error(odin_parse_(quote(dim <- 1)),
               "Reserved name", class = "odin_error")
  expect_error(odin_parse_(quote(user <- 1)),
               "Reserved name", class = "odin_error")

  expect_error(odin_parse_(quote(deriv_x <- 1)),
               "Variable name cannot start with 'deriv_'",
               class = "odin_error")
  expect_error(odin_parse_(quote(dim_x <- 1)),
               "Variable name cannot start with 'dim_'",
               class = "odin_error")
  expect_error(odin_parse_(quote(initial_x <- 1)),
               "Variable name cannot start with 'initial_'",
               class = "odin_error")
})


test_that("compatible rhs", {
  expect_error(odin_parse("deriv(y) = 1; initial(x) = 2"),
               "must contain same set of equations",
               class = "odin_error")
  expect_error(odin_parse("update(y) = 1; initial(x) = 2"),
               "must contain same set of equations",
               class = "odin_error")

  expect_error(odin_parse(
    "deriv(y) = 1; update(z) = 1; initial(y) = 1; initial(z) = 1;"),
    "Cannot mix deriv() and update()", fixed = TRUE, class = "odin_error")
})


test_that("unknown variables", {
  expect_error(odin_parse_(
    ex("x <- y + z")),
    "Unknown variables y, z",
    class = "odin_error")
})


test_that("variables cannot be assigned to", {
  expect_error(odin_parse("deriv(y) = 1\ninitial(y) = 1\ny <- 1"),
               "variables on lhs must be within deriv",
               class = "odin_error")
})


test_that("duplicate non-array entries", {
  expect_error(
    odin_parse_(ex("x = 1\nx = 2")),
    "Duplicate entries must all be array assignments",
    class = "odin_error")
  expect_error(
    odin_parse_(ex("x[1] = 1\nx = 2")),
    "Duplicate entries must all be array assignments",
    class = "odin_error")
})


test_that("missing dim", {
  expect_error(
    odin_parse("deriv(x[1]) = 1\ninitial(x) = 2"),
    "Missing dim() call", fixed = TRUE, class = "odin_error")
})


test_that("array dimensionality must be consistent", {
  expect_error(
    odin_parse_(ex("x[1] <- 1\nx[2,1] <- 2\ndim(x) <- 10")),
    "Array dimensionality is not consistent", class = "odin_error")
  expect_error(
    odin_parse_(ex("x[1] <- 1\ny <- x[2,1]\ndim(x) <- 10")),
    "Incorrect dimensionality for 'x'", class = "odin_error")
})


test_that("dim lhs must be simple", {
  expect_error(odin_parse_(quote(dim(x[1]) <- 1)),
               "must be applied to a name only",
               class = "odin_error")
  expect_error(odin_parse_(quote(dim(x[1,2]) <- c(1, 2))),
               "must be applied to a name only",
               class = "odin_error")
})


test_that("user usage", {
  expect_error(odin_parse("x <- user(a, b)"),
               "Only first argument to user() may be unnamed",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse("x <- user(a, c = 1)"),
               "Unknown argument to user(): 'c'",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse_(quote(x <- user(user(2)))),
               "user() call must not use functions",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse_(quote(x <- user(a))),
               "user() call must not reference variables",
               fixed = TRUE, class = "odin_error")
})


test_that("array usage", {
  expect_error(odin_parse_(ex("y <- x\nx[1] <- 1\ndim(x) <- 10")),
               "Array 'x' used without array index", class = "odin_error")
})


test_that("dim rhs must be simple", {
  expect_error(odin_parse_(ex("y[] <- 0\ndim(y) <- f(p)")),
               "Invalid dim() rhs", fixed = TRUE, class = "odin_error")
})


test_that("user call", {
  expect_error(odin_parse_(quote(a <- user() + 1)),
               "user() must be the only call on the rhs",
               fixed = TRUE, class = "odin_error")
})


test_that("interpolate call", {
  expect_error(odin_parse_(quote(a <- interpolate() + 1)),
               "interpolate() must be the only call on the rhs",
               fixed = TRUE, class = "odin_error")
})


test_that("deriv can't be used as rhs symbol", {
  expect_error(odin_parse_(ex("a <- deriv")),
               "Function 'deriv' is disallowed as symbol on rhs",
               class = "odin_error")
})


test_that("unclassifiable statement", {
  expect_error(odin_parse_(quote(x <- 1(2))),
               "Cannot process statement", class = "odin_error")
})


test_that("RHS array checking", {
  ## Dummy args:
  eq <- list(source = 1)
  expr <- quote(x)
  ia <- character(0)
  source <- "x"

  ## This needs expanding as I find more corner cases
  ##
  ## TODO: I would prefer this to go all the way from odin_parse
  expect_null(ir_parse_arrays_check_rhs(quote(a + b[1]), c(b = 1), ia,
                                        eq, source))
  expect_error(ir_parse_arrays_check_rhs(quote(a + b[1]), c(b = 2), ia,
                                         eq, source),
               "Incorrect dimensionality for 'b'", class = "odin_error")
  expect_error(ir_parse_arrays_check_rhs(quote(a + b[1,2,3]), c(b = 2), ia,
                                         eq, source),
               "Incorrect dimensionality for 'b'", class = "odin_error")
  expect_null(ir_parse_arrays_check_rhs(quote(a + b[1,2,3]), c(b = 3), ia,
                                        eq, source))
  expect_error(ir_parse_arrays_check_rhs(quote(a + b[f(1)]), c(b = 1), ia,
                                         eq, source),
               "Disallowed functions used for b", class = "odin_error")
  expect_error(ir_parse_arrays_check_rhs(quote(b), c(b = 1), ia,
                                         eq, source),
               "Array 'b' used without array index", class = "odin_error")
  expect_null(ir_parse_arrays_check_rhs(quote(a), c(b = 1), ia,
                                        eq, source))
  expect_error(ir_parse_arrays_check_rhs(quote(a[]), c(a = 1), ia,
                                         eq, source),
               "Empty array index not allowed on rhs", class = "odin_error")

  rhs <- ir_parse_expr_rhs_expression_sum(quote(sum(a)))
  expect_null(ir_parse_arrays_check_rhs(rhs, c(a = 1), ia, eq, source))
  expect_error(ir_parse_arrays_check_rhs(rhs, c(b = 1), ia, eq, source),
               "Function 'sum' requires array as argument 1",
               class = "odin_error")

  rhs <- ir_parse_expr_rhs_expression_sum(quote(sum(a[])))
  expect_error(ir_parse_arrays_check_rhs(rhs, c(b = 1), ia, eq, source),
               "Function 'sum' requires array as argument 1",
               class = "odin_error")

  expr <- quote(sum(a[,]))
  rhs <- ir_parse_expr_rhs_expression_sum(expr)
  expect_error(ir_parse_arrays_check_rhs(rhs, c(a = 1), ia, eq, source),
               "Incorrect dimensionality for 'a' in 'sum' (expected 1)",
               fixed = TRUE, class = "odin_error")
  expect_silent(ir_parse_arrays_check_rhs(rhs, c(a = 2), ia, eq, source))
  expect_error(ir_parse_arrays_check_rhs(rhs, c(a = 3), ia, eq, source),
               "Incorrect dimensionality for 'a' in 'sum' (expected 3)",
               fixed = TRUE, class = "odin_error")
})

test_that("lhs array checking", {
  res <- ir_parse_expr_lhs_check_index(quote(a + (2:(n-3) - 4) + z))
  expect_true(res)
  expect_equal(attr(res, "value_max"), quote(a + ((n-3) - 4) + z))
  expect_equal(attr(res, "value_min"), quote(a + (2 - 4) + z))

  res <- ir_parse_expr_lhs_check_index(quote(a))
  expect_true(res)
  expect_equal(attr(res, "value_max"), quote(a))
  expect_null(attr(res, "value_min"))

  expect_false(ir_parse_expr_lhs_check_index(quote(a:b + c:d)))
  expect_false(ir_parse_expr_lhs_check_index(quote(-(a:b))))
  expect_false(ir_parse_expr_lhs_check_index(quote((a:b):c)))
  expect_false(ir_parse_expr_lhs_check_index(quote(c:(a:b))))
  expect_false(ir_parse_expr_lhs_check_index(quote((-a))))
})

test_that("sum rewriting", {
  ## This form is not rewritten:
  expect_identical(ir_parse_expr_rhs_expression_sum(quote(sum(a))),
                   quote(sum(a)))
  expect_error(odin_parse("x <- sum(a, b)"),
               "Expected 1 argument in sum call", class = "odin_error")

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
    odin_parse("y <- if (foo) 1"),
    "All if statements must have an else clause", class = "odin_error")
  expect_error(
    odin_parse("y <- 1 + (if (foo) 1) + bar"),
    "All if statements must have an else clause", class = "odin_error")
})

test_that("recursive variables", {
  expect_error(odin_parse_(quote(foo <- foo + 1)),
               "Self referencing expressions not allowed",
               class = "odin_error")
})

test_that("array extent and time", {
  expect_error(odin_parse_(quote({
    deriv(y[]) <- 1
    initial(y[]) <- 0
    dim(y) <- t
  })), "Array extent is determined by time", class = "odin_error")

  expect_error(odin_parse_(quote({
    deriv(y[]) <- 1
    initial(y[]) <- 0
    a <- t
    dim(y) <- a
  })), "Array extent is determined by time", class = "odin_error")

  expect_error(odin_parse_(quote({
    deriv(y[]) <- 1
    initial(y[]) <- 0
    deriv(z) <- 1
    initial(z) <- 0
    dim(y) <- z
  })), "Array extent is determined by time", class = "odin_error")
})

test_that("lhs checking", {
  expect_error(odin_parse_(quote(1 <- 1)),
               "Invalid left hand side", class = "odin_error")

  expect_error(odin_parse_(quote(1 + 1 <- 1)),
               "Unhandled expression + on lhs",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse_(quote(devs(a) <- 1)),
               "Unhandled expression devs on lhs",
               fixed = TRUE, class = "odin_error")

  expect_error(odin_parse_(quote(deriv(a, b) <- 1)),
               "Invalid length special function on lhs", class = "odin_error")
  expect_error(odin_parse_(quote(initial(a, b) <- 1)),
               "Invalid length special function on lhs", class = "odin_error")
  expect_error(odin_parse_(quote(dim(a, b) <- 1)),
               "Invalid length special function on lhs", class = "odin_error")

  expect_error(odin_parse_(quote(deriv() <- 1)),
               "Invalid length special function on lhs", class = "odin_error")
  expect_error(odin_parse_(quote(initial() <- 1)),
               "Invalid length special function on lhs", class = "odin_error")
  expect_error(odin_parse_(quote(dim() <- 1)),
               "Invalid length special function on lhs", class = "odin_error")

  expect_error(odin_parse_(quote(deriv(deriv(a)) <- 1)),
               "Invalid nested lhs function usage", class = "odin_error")
  expect_error(odin_parse_(quote(deriv(initial(a)) <- 1)),
               "Invalid nested lhs function usage", class = "odin_error")
  expect_error(odin_parse_(quote(deriv(dim(a)) <- 1)),
               "Invalid nested lhs function usage", class = "odin_error")
  expect_error(odin_parse_(quote(initial(deriv(a)) <- 1)),
               "Invalid nested lhs function usage", class = "odin_error")
  expect_error(odin_parse_(quote(initial(initial(a)) <- 1)),
               "Invalid nested lhs function usage", class = "odin_error")
  expect_error(odin_parse_(quote(initial(dim(a)) <- 1)),
               "Invalid nested lhs function usage", class = "odin_error")
  expect_error(odin_parse_(quote(dim(deriv(a)) <- 1)),
               "Invalid nested lhs function usage", class = "odin_error")
  expect_error(odin_parse_(quote(dim(initial(a)) <- 1)),
               "Invalid nested lhs function usage", class = "odin_error")
  expect_error(odin_parse_(quote(dim(dim(a)) <- 1)),
               "Invalid nested lhs function usage", class = "odin_error")
})


test_that("interpolation", {
  expect_error(odin_parse_(quote(x <- interpolate(a, b, c))),
               "Expected a string constant for interpolation type",
               class = "odin_error")
  expect_error(odin_parse_(quote(x <- interpolate(a, b, 1L))),
               "Expected a string constant for interpolation type",
               class = "odin_error")
  expect_error(odin_parse_(quote(x <- interpolate(a, b, "lin"))),
               "Invalid interpolation type",
               class = "odin_error")

  expect_error(odin_parse_(quote(x <- interpolate(a))),
               "interpolate() requires two or three arguments",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse_(quote(x <- interpolate(a, b, c, d))),
               "interpolate() requires two or three arguments",
               fixed = TRUE, class = "odin_error")

  expect_error(odin_parse_(quote(x <- interpolate(2, x))),
               "interpolation time argument must be a symbol",
               class = "odin_error")
  expect_error(odin_parse_(quote(x <- interpolate(x, 2))),
               "interpolation target argument must be a symbol",
               class = "odin_error")
})

test_that("sums", {
  expect_error(odin_parse_(quote(x <- sum(1 + 2))),
               "Argument to sum must be a symbol or indexed array",
               class = "odin_error")
  expect_error(odin_parse_(quote(x <- sum(1))),
               "Argument to sum must be a symbol or indexed array",
               class = "odin_error")
  expect_error(odin_parse_(quote(x <- sum(a, b))),
               "Expected 1 argument in sum call, but recieved 2",
               class = "odin_error")
  expect_error(odin_parse_(quote(x <- sum())),
               "Expected 1 argument in sum call, but recieved 0",
               class = "odin_error")

  expect_error(odin_parse_(quote(x <- sum(a[f(b)]))),
               "Invalid array use in sum",
               class = "odin_error")
  expect_error(odin_parse_(quote(x <- sum(a[f(b), c]))),
               "Invalid array use in sum",
               class = "odin_error")
  expect_error(odin_parse_(quote(x <- sum(a[f(b), f(c)]))),
               "Invalid array use in sum",
               class = "odin_error")
})


test_that("some dim() pathologies", {
  expect_error(odin_parse_(ex("a[] <- user(); dim(a) <- user(1)")),
               "Default in user dimension size not handled",
               class = "odin_error")
  expect_error(odin_parse_(ex("a[] <- user(); dim(a) <- user(min = 2)")),
               "min and max are not supported for user dimensions",
               class = "odin_error")
  expect_error(odin_parse_(ex("a[] <- user(); dim(a) <- user(max = 2)")),
               "min and max are not supported for user dimensions",
               class = "odin_error")
  expect_error(odin_parse_(ex("dim(a) <- 'foo'")),
               "expected numeric, symbol, user or c",
               class = "odin_error")
  expect_error(odin_parse_(ex("dim(a) <- NULL")),
               "expected numeric, symbol, user or c",
               class = "odin_error")
  expect_error(odin_parse_(ex("dim(a) <- c(1, 'foo')")),
               "must contain symbols, numbers or lengths",
               class = "odin_error")
  expect_error(odin_parse_(ex("dim(a) <- c(1, c(2, 3))")),
               "must contain symbols, numbers or lengths",
               class = "odin_error")
  expect_error(odin_parse_(ex("dim(a) <- c(1, NULL)")),
               "must contain symbols, numbers or lengths",
               class = "odin_error")
  expect_error(odin_parse_(ex("dim(a) <- c(1 + 1, 1)")),
               "must contain symbols, numbers or lengths",
               class = "odin_error")
})


test_that("array pathologies", {
  expect_error(
    odin_parse_(ex("dim(y) <- 2; y <- 1")),
    "Array variables must always assign as arrays",
    class = "odin_error")
  expect_error(
    odin_parse_(ex("dim(y) <- user(); y <- 1")),
    "Array variables must always assign as arrays",
    class = "odin_error")
  expect_error(
    odin_parse_(ex("dim(y) <- 2; y[1] <- user(); y[2] <- 1")),
    "user() may only be used on a single-line array",
    fixed = TRUE, class = "odin_error")
})

test_that("correct dim() use", {
  expect_error(odin_parse("dim(x) <- 10; a <- length(x, 1)"),
               "Expected 1 argument in length",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse("dim(x) <- 10; a <- length()"),
               "Expected 1 argument in length",
               fixed = TRUE, class = "odin_error")

  expect_error(odin_parse_(ex("a <- length(1)")),
               "argument to length must be a symbol",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse_(ex("x <- 2; a <- length(x)")),
               "argument to length must be an array",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse_(ex(
    "dim(x) <- c(2, 10); x[, ] <- 1; a <- length(x)")),
    "argument to length must be a 1-D array",
    fixed = TRUE, class = "odin_error")

  expect_error(odin_parse("dim(x) <- 10; a <- dim(x)"),
               "Expected 2 arguments in dim call",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse("dim(x) <- 10; a <- dim(x, 1, 2)"),
               "Expected 2 arguments in dim call",
               fixed = TRUE, class = "odin_error")

  expect_error(odin_parse_(ex("a <- dim(1, x)")),
               "first argument to dim must be a symbol",
               fixed = TRUE, class = "odin_error")

  expect_error(odin_parse_(ex("x <- 2; a <- dim(x, 1)")),
               "first argument to dim must be an array",
               fixed = TRUE, class = "odin_error")

  expect_error(odin_parse_(ex(
    "dim(x) <- 1; x[] <- 1; a <- dim(x, 1)")),
    "dim() must not be used for 1D arrays",
    fixed = TRUE, class = "odin_error")
  expect_error(odin_parse_(ex(
    "dim(x) <- c(1, 2); x[,] <- 1; a <- dim(x, 1.4)")),
    "second argument to dim() must be an integer",
    fixed = TRUE, class = "odin_error")
  expect_error(odin_parse_(ex(
    "dim(x) <- c(1, 2); x[,] <- 1; a <- dim(x, b)")),
    "second argument to dim() must be an integer",
    fixed = TRUE, class = "odin_error")

  expect_error(odin_parse_(ex(
    "dim(x) <- c(1, 2); x[,] <- 1; a <- dim(x, 3)")),
    "array index out of bounds",
    fixed = TRUE, class = "odin_error")

  expect_error(odin_parse_(ex("dim(x) <- 10; b <- length(x)")),
               "Array variable x is never assigned", class = "odin_error")
})

test_that("check array rhs", {
  expect_error(
    odin_parse_(ex("dim(x) <- 10; x[] <- 1; b <- x[]")),
    "Empty array index not allowed on rhs", class = "odin_error")

  expect_error(
    odin_parse_(ex("dim(x) <- 10; x[] <- 1; a <- x[x]")),
    "Disallowed variables used for x", class = "odin_error")

  expect_error(
    odin_parse_(ex("dim(x) <- 10; x[] <- 1; y <- 1; a <- x[1] + y[1];")),
    "Unknown array variable y in", class = "odin_error")
})

## Probably more needed here as there are some special cases...
test_that("cyclic dependency", {
  expect_error(
    odin_parse_(ex("x <- y; y <- x")),
    "A cyclic dependency detected")
  expect_error(
    odin_parse_(ex("x <- y; y <- z; z <- x")),
    "A cyclic dependency detected")
})

test_that("range operator on RHS", {
  expect_error(odin_parse("y <- x[1:2]"),
               "Range operator ':' may not be used on rhs",
               class = "odin_error")
})

test_that("rewrite errors", {
  expect_error(odin_parse_(ex("a <- 1; x <- foo(a)")),
               "Unsupported function: foo",
               class = "odin_error")
  expect_error(odin_parse_(ex("a <- 1; x <- foo(a); y <- bar(a)")),
               "Unsupported functions: foo, bar",
               class = "odin_error")
  expect_error(odin_parse_(ex("a <- 1; x <- abs(a, 1)")),
               "Expected 1 argument in abs call",
               class = "odin_error")
  expect_error(odin_parse_(ex("a <- 1; x <- min(a)")),
               "Expected 2 or more arguments in min call",
               class = "odin_error")
})

test_that("Incomplete user array", {
  expect_error(odin_parse_(quote({
    initial(x[,]) <- x0[i, j]
    deriv(x[,]) <- 1
    dim(x0) <- user()
    dim(x) <- c(dim(x0, 1), dim(x0, 1))
  })),
  "No array assignment found for x0", class = "odin_error")

  expect_error(
    odin_parse_(ex("dim(x) <- user()")),
    "No array assignment found for x", class = "odin_error")
})


test_that("output name collision", {
  expect_error(
    gen <- odin_parse({
      deriv(y) <- 1
      initial(y) <- 1
      output(y) <- 1
    }),
    "same as variable name", class = "odin_error")
})


test_that("invalid self output", {
  expect_error(odin_parse({
    deriv(y[]) <- r[i] * y[i]
    initial(y[]) <- 1
    r[] <- 0.1
    dim(r) <- 3
    dim(y) <- 3
    ## This should fail:
    output(r[]) <- r
    output(r[]) <- 1
  }),
  "direct output may only be used on a single-line array",
  fixed = TRUE, class = "odin_error")
})


test_that("user sized variables not allowed", {
  expect_error(odin_parse({
    deriv(y[]) <- r * y[i]
    initial(y[]) <- 1
    r <- 0.1
    dim(y) <- user()
  }),
  "Can't specify user-sized variables", class = "odin_error")
})


test_that("taking size of non-array variable is an error", {
  expect_error(odin_parse({
    deriv(y) <- 1
    initial(y) <- 1
    x <- length(y)
  }),
  "argument to length must be an array", class = "odin_error")

  expect_error(odin_parse({
    deriv(y) <- 1
    initial(y) <- 1
    x <- dim(y, 2)
  }),
  "argument to dim must be an array", class = "odin_error")
})


test_that("dependent dim never assigned", {
  ## I have no idea how common this is, but this is to prevent a
  ## regression.
  ##
  ## The issue here is that we say that dim(r) is dependent on dim(y0)
  ## but we never actually assign it, so we don't *know* that it's a
  ## 1d array or not.
  expect_error(
    gen <- odin_parse({
      deriv(y[]) <- y[i] * r[i]
      initial(y[]) <- y0[i]
      dim(y) <- length(y0)
      dim(r) <- length(y0)
      y0[] <- user()
      dim(y0) <- user()
    }),
    "Array variable r is never assigned; can't work out rank",
    class = "odin_error")
})


test_that("detect integers", {
  ir <- odin_parse({
    n <- 2
    m <- 2
    deriv(S[, ]) <- 0
    deriv(I) <- S[n,m]
    dim(S) <- c(n,m)
    initial(S[, ]) <- S0[i, j]
    initial(I) <- 0
    S0[, ] <- user()
    dim(S0) <- c(n, m)
  })
  dat <- ir_deserialise(ir)
  type <- vcapply(dat$data$elements, "[[", "storage_type")
  int <- names_if(type == "int")
  expect_true("n" %in% int)
  expect_true("m" %in% int)
})


test_that("notify on naked index", {
  code <- c(
    "deriv(x[]) <- i",
    "initial(x[]) <- 1",
    "dim(x) <- 5")
  expect_message(
    odin_parse(code),
    "Equations use index variables i on the rhs outside of an index.")
  expect_message(
    odin_parse(code, options = odin_options(no_check_naked_index = TRUE)),
    NA)
})


test_that("no variables", {
  expect_error(odin_parse({
    x <- 1
  }),
  "Did not find a deriv() or an update() call",
  fixed = TRUE, class = "odin_error")
})


test_that("dim on rhs", {
  expect_error(odin_parse({
      deriv(y[,]) <- r[i] * y[i, j]
      initial(y[,]) <- 1
      r[] <- 0.1
      n <- 1
      dim(y) <- c(3, 4)
      dim(r) <- dim(y, n)
  }),
  "Invalid dim call; expected integer second argument", class = "odin_error")
})


test_that("skip sum in naked index check", {
  ## In the first version of the naked index check, this produced a
  ## note because the sum expression expands out to
  ##
  ##   odin_sum(m, i, i, 1, dim(m, 2))
  ##
  ## which looks like a naked index.
  expect_silent(
    odin_parse({
      deriv(y) <- sum(v)
      initial(y) <- 1
      m[,] <- user()
      v[] <- sum(m[i, ])
      dim(m) <- c(4, 4)
      dim(v) <- 4
    }))
})


test_that("skip some equality operations in naked index check", {
  ## This turns up in lily's model
  expect_silent(odin_parse({
    deriv(y) <- 1
    initial(y) <- sum(m)
    m[, ] <- if (i == j) 1 else 0
    dim(m) <- c(4, 4)
  }))

  expect_message(odin_parse({
    deriv(y) <- 1
    initial(y) <- sum(m)
    m[, ] <- if (i == 1) 1 else 0
    dim(m) <- c(4, 4)
  }), "Equations use index variables i on the rhs outside of an index")

  expect_message(odin_parse({
    deriv(y) <- 1
    initial(y) <- sum(m)
    m[, ] <- if (i == j + 0) 1 else 0
    dim(m) <- c(4, 4)
  }), "Equations use index variables i, j on the rhs outside of an index")
})


## issue #112
test_that("sensible error message with invalid input", {
  expect_error(
    odin_parse({
      update(x) <- 1
      initial(x) <- 1
      fn <- function(x) x + 1
    }),
    "Cannot define R functions in odin model", class = "odin_error")
})


## issue #166
test_that("can't use array indices that exceed the rank of the lhs", {
  expect_error(
    odin_parse({
      ## A simplified version of Anne's problem:
      m[,] <- user()
      r[] <- m[i, 1] + m[1, j]
      dim(m) <- user()
      dim(r) <- 5
      ## Just here because we need something:
      initial(x) <- 1
      update(x) <- 1
    }),
    "Index variable 'j' not possible for array of rank 1",
    fixed = TRUE, class = "odin_error")

  expect_error(
    odin_parse({
      m[,] <- user()
      r[] <- m[k, 1] + m[1, j]
      dim(m) <- user()
      dim(r) <- 5
      ## Just here because we need something:
      initial(x) <- 1
      update(x) <- 1
    }),
    "Index variable 'j', 'k' not possible for array of rank 1",
    fixed = TRUE, class = "odin_error")
})
