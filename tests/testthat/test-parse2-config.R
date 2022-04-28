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

test_that("config(base)", {
  expect_error(odin_parse("config(base) <- 'foo'; config(base) <- 'foo'"),
               "Expected a single config(base) option",
               fixed = TRUE, class = "odin_error")
  expect_error(odin_parse("config(base) <- foo;"),
               "Expected a character for config(base) but recieved a symbol",
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
  skip_for_compilation()
  options <- odin_options(target = "c")
  expect_error(odin_parse_(quote(config(include) <- 1), options),
               "Expected a character for config(include)",
               fixed = TRUE, class = "odin_error")

  expect_error(odin_parse_(quote(config(include) <- "no file.c"), options),
               "Could not find file 'no file.c'",
               fixed = TRUE, class = "odin_error")

  expect_error(odin_parse(
    'config(include) <- "user_fns.c"; config(include) <- "user_fns.c"',
    options),
    "Duplicated function 'squarepulse' while reading includes",
    class = "odin_error")
})


test_that("Can include multiple files", {
  skip_for_compilation()
  ir <- odin_parse({
    config(include) <- "user_fns.c"
    config(include) <- "identity.c"
    initial(x) <- 1
    deriv(x) <- 1
  })
  dat <- ir_deserialise(ir)
  expect_length(dat$config$include, 2)
  expect_equal(
    vcapply(dat$config$include$data, function(x) basename(x$filename[[1]])),
    c("user_fns.c", "identity.c"))
})


test_that("extend config", {
  options <- odin_options(target = "c")
  options$config_custom <- "a"

  ir <- odin_parse({
    config(a) <- 1
    initial(x) <- 1
    deriv(x) <- 1
  }, options = options)
  expect_equal(ir_deserialise(ir)$config$custom,
               list(list(name = "a", value = 1)))

  ir <- odin_parse({
    config(a) <- 1
    config(a) <- 2
    initial(x) <- 1
    deriv(x) <- 1
  }, options = options)
  expect_equal(ir_deserialise(ir)$config$custom,
               list(list(name = "a", value = 1),
                    list(name = "a", value = 2)))

  expect_error(
    odin_parse({
      config(a) <- 1
      config(b) <- 2
      initial(x) <- 1
      deriv(x) <- 1
    }, options = options),
    "Unknown configuration option: b")
})
