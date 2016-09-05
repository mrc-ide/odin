context("parse (config)")

test_that("invalid options", {
  expect_error(odin_parse("config(a) <- 1;"),
               "Unknown configuration options: a")
  expect_error(odin_parse("config(a) <- 1; config(b) <- 2"),
               "Unknown configuration options: a, b")
})

test_that("config() takes a symbol", {
  expect_error(odin_parse("config('base') <- 1;"),
               "Argument to config must be a symbol or expression")
})

test_that("config() rhs is atomic", {
  expect_error(odin_parse("config(base) <- 1 + 1;"),
               "rhs must be atomic")
})

test_that("config(base)", {
  expect_error(odin_parse("config(base) <- 'foo'; config(base) <- 'foo'"),
               "Expected a single config(base) option", fixed=TRUE)
  expect_error(odin_parse("config(base) <- foo;"),
               "config() rhs must be atomic", fixed=TRUE)
  expect_error(odin_parse("config(base) <- 1;"),
               "config(base) must be a character", fixed=TRUE)
  ## some invalid identifiers:
  expect_error(odin_parse("config(base) <- '1foo';"),
               "must be a valid C identifier", fixed=TRUE)
  expect_error(odin_parse("config(base) <- '*foo';"),
               "must be a valid C identifier", fixed=TRUE)
  expect_error(odin_parse("config(base) <- '-foo';"),
               "must be a valid C identifier", fixed=TRUE)
  expect_error(odin_parse("config(base) <- '.foo';"),
               "must be a valid C identifier", fixed=TRUE)
})

test_that("config(include)", {
  expect_error(odin_parse(quote(config(include) <- 1)),
               "config(include) must be a character", fixed=TRUE)

  expect_error(odin_parse(quote(config(include) <- "no file.c")),
               "File 'no file.c' does not exist", fixed=TRUE)

  expect_error(odin_parse(
    'config(include) <- "user_fns.c"; config(include) <- "user_fns.c"'),
    "Duplicate declarations while reading includes")
})
