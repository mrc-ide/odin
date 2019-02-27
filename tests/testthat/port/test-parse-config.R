context("parse (config)")

test_that("invalid options", {
  expect_error(odin_parse2("config(a) <- 1;"),
               "Unknown configuration option: a")
})

test_that("config() takes a symbol", {
  expect_error(odin_parse2("config('base') <- 1;"),
               "Argument to config must be a symbol")
})

test_that("config() rhs is atomic", {
  expect_error(odin_parse2("config(base) <- 1 + 1;"),
               "config() rhs must be atomic (not an expression", fixed = TRUE)
})

test_that("config(base)", {
  expect_error(odin_parse2("config(base) <- 'foo'; config(base) <- 'foo'"),
               "Expected a single config(base) option", fixed = TRUE)
  expect_error(odin_parse2("config(base) <- foo;"),
               "config() rhs must be atomic", fixed = TRUE)
  expect_error(
    odin_parse2("config(base) <- 1;"),
    "Expected a character for config(base) but recieved a double",
    fixed = TRUE)

  ## some invalid identifiers:
  expect_error(odin_parse2("config(base) <- '1foo';"),
               "must be a valid C identifier", fixed = TRUE)
  expect_error(odin_parse2("config(base) <- '*foo';"),
               "must be a valid C identifier", fixed = TRUE)
  expect_error(odin_parse2("config(base) <- '-foo';"),
               "must be a valid C identifier", fixed = TRUE)
  expect_error(odin_parse2("config(base) <- '.foo';"),
               "must be a valid C identifier", fixed = TRUE)
})

test_that("config(include)", {
  skip("user_c")
  expect_error(odin_parse2(quote(config(include) <- 1)),
               "config(include) must be a character", fixed = TRUE)

  expect_error(odin_parse2(quote(config(include) <- "no file.c")),
               "Could not find file 'no file.c'", fixed = TRUE)

  expect_error(odin_parse2(
    'config(include) <- "user_fns.c"; config(include) <- "user_fns.c"'),
    "Duplicate declarations while reading includes")
})
