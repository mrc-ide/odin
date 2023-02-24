test_that("Can parse glue expressions from a string", {
  expect_equal(debug_parse_string("{a}"), "a")
  expect_equal(debug_parse_string("a"), character(0))
  expect_equal(debug_parse_string("{a} {b} {c}"), c("a", "b", "c"))
})


test_that("Can re-substitute values into a string", {
  expect_equal(debug_substitute_string("{a}", 1), "1")
  expect_equal(debug_substitute_string("a", character()), "a")
  expect_equal(debug_substitute_string("{a} {b} {c}", c("1", "2", "3")),
               "1 2 3")
  expect_equal(debug_substitute_string("{a;2f} {b} {c}", c("1", "2", "3")),
               "1 2 3")
})


test_that("Can parse debug element", {
  deps <- function(vars, fns = NULL) {
    list(functions = fns %||% character(0), variables = vars)
  }
  
  expect_equal(
    debug_parse_element("x"),
    list(expr = quote(x), depends = deps("x"), format = NULL))
  expect_equal(
    debug_parse_element("x; f"),
    list(expr = quote(x), depends = deps("x"), format = "f"))
  expect_equal(
    debug_parse_element("x + y; f"),
    list(expr = quote(x + y), depends = deps(c("x", "y"), "+"), format = "f"))
})


test_that("Can process print call", {
  src <- letters
  line <- 5
  expect_equal(
    debug_parse_print_call(list("str"), line, letters),
    list(type = "print", expr = "str", when = NULL))
  expect_error(
    debug_parse_print_call(list(msg = "str"), line, letters),
    "print() expects the first argument to be unnamed",
    fixed = TRUE, class = "odin_error")
  expect_error(
    debug_parse_print_call(10, line, letters),
    "print() requires a string argument",
    fixed = TRUE, class = "odin_error")


  expect_equal(
    debug_parse_print_call(list("str", when = TRUE), line, letters),
    list(type = "print", expr = "str", when = TRUE))
  expect_error(
    debug_parse_print_call(list("str", TRUE), line, letters),
    "print() expects every argument but the first to be named",
    fixed = TRUE, class = "odin_error")

  expect_error(
    debug_parse_print_call(list("str", other = TRUE), line, letters),
    "Unknown argument to print(): 'other'",
    fixed = TRUE, class = "odin_error")
  expect_error(
    debug_parse_print_call(list("str", a = TRUE, b = FALSE), line, letters),
    "Unknown argument to print(): 'a', 'b'",
    fixed = TRUE, class = "odin_error")
})


test_that("Can process a model with debug printing", {
  ir <- odin_parse({
    deriv(x) <- 1
    initial(x) <- 0
    print("x: {x}")
  })
  dat <- ir_deserialise(ir)
  expct_true(dat$features$has_debug)
  expect_length(dat$debug, 1)
  expect_equal(dat$debug[[1]]$type, "print")
  expect_equal(dat$debug[[1]]$format, "x: %f")
  expect_equal(dat$debug[[1]]$args, list("x"))
  expect_equal(dat$debug[[1]]$depends,
               list(functions = character(), variables = "x"))
  expect_null(dat$debug[[1]]$when)
})


test_that("Require that debug string contains at least one variable", {
  expect_error(
    odin_parse({
      deriv(x) <- 1
      initial(x) <- 0
      print("x: %f")
    }),
    "Invalid print() expression does not reference any values",
    fixed = TRUE, class = "odin_error")
})


test_that("Handle parse failure gracefully", {
  skip_on_cran() # somewhat platform specific
  err <- tryCatch(sprintf("%z"), error = identity)
  expect_error(
    odin_parse({
      deriv(x) <- 1
      initial(x) <- 0
      print("x: {x; z}")
    }),
    paste("Failed to parse debug string 'x; z':", err$msg),
    fixed = TRUE, class = "odin_error")
})


test_that("Error if we reference unknown variables in print", {
  expect_error(
    odin_parse({
      deriv(x) <- 1
      initial(x) <- 0
      print("x: {z}")
    }),
    "Unknown variable 'z' in print()",
    fixed = TRUE, class = "odin_error")
  expect_error(
    odin_parse({
      deriv(x) <- 1
      initial(x) <- 0
      print("{x} - {y} - {z}")
    }),
    "Unknown variable 'y', 'z' in print()",
    fixed = TRUE, class = "odin_error")
})
