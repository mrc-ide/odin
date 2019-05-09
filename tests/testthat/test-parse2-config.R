context("parse: config")

test_that("invalid options", {
  expect_error(odin_parse("config(a) <- 1;"),
               "Unknown configuration option: a",
               class = "odin_error")
})

test_that("config() takes a symbol", {
  expect_error(odin_parse("config('base') <- 1;"),
               "Argument to config must be a symbol",
               class = "odin_error")
})

test_that("config() rhs is atomic", {
  expect_error(odin_parse("config(base) <- 1 + 1;"),
               "config() rhs must be atomic (not an expression",
               fixed = TRUE, class = "odin_error")
})

test_that("config(base)", {
  expect_error(odin_parse("config(base) <- 'foo'; config(base) <- 'foo'"),
               "Expected a single config(base) option",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse("config(base) <- foo;"),
               "config() rhs must be atomic",
               fixed = TRUE, class = "odin_error")
  expect_error(
    odin_parse("config(base) <- 1;"),
    "Expected a character for config(base) but recieved a double",
    fixed = TRUE, class = "odin_error")

  ## some invalid identifiers:
  expect_error(odin_parse("config(base) <- '1foo';"),
               "must be a valid C identifier",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse("config(base) <- '*foo';"),
               "must be a valid C identifier",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse("config(base) <- '-foo';"),
               "must be a valid C identifier",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse("config(base) <- '.foo';"),
               "must be a valid C identifier",
               fixed = TRUE, class = "odin_error")
})

test_that("config(include)", {
  expect_error(odin_parse_(quote(config(include) <- 1)),
               "Expected a character for config(include)",
               fixed = TRUE, class = "odin_error")

  expect_error(odin_parse_(quote(config(include) <- "no file.c")),
               "Could not find file 'no file.c'",
               fixed = TRUE, class = "odin_error")

  expect_error(odin_parse(
    'config(include) <- "user_fns.c"; config(include) <- "user_fns.c"'),
    "Duplicate declarations while reading includes",
    class = "odin_error")
})
