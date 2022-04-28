context("interface")

test_that("NSE and SE defaults are the same", {
  expect_equal(formals(odin), formals(odin_))
})

test_that("verbose", {
  skip_on_cran()
  expect_output(
    odin({
      initial(x) <- 0
      update(x) <- x + norm_rand()
      config(base) <- "mycrazymodel"
    }, target = "c", workdir = tempfile(), skip_cache = TRUE, verbose = TRUE),
    "mycrazymodel[[:xdigit:]]{8}")
})

test_that("n_history is configurable", {
  gen <- odin({
    ylag <- delay(y, 10)
    initial(y) <- 0.5
    deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
  })

  mod <- gen$new(use_dde = TRUE)
  expect_error(mod$run(seq(0, 200), n_history = 0),
               "Integration failure: can't use ylag in model with no history")

  mod <- gen$new(use_dde = FALSE)
  ## Don't test for precice deSolve error message; just test fail/pass
  expect_error(mod$run(seq(0, 200), n_history = 1))
  expect_error(mod$run(seq(0, 200), n_history = 1000), NA)
})


test_that("sensible error on empty input", {
  path <- tempfile()
  writeLines("", path)
  expect_error(odin_(path),
               "Did not find a deriv() or an update() call",
               fixed = TRUE, class = "odin_error")
  writeLines("# some comment", path)
  expect_error(odin_(path),
               "Did not find a deriv() or an update() call",
               fixed = TRUE, class = "odin_error")
})


test_that("prevent unknown target", {
  expect_error(odin({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2
  }, target = "fortran"),
  "Unknown target 'fortran'", fixed = TRUE)
})


## issue #88
test_that("force a vector of strings (compile)", {
  gen <- odin(c("deriv(y) <- 0.5", "initial(y) <- 1"), target = "r")
  mod <- gen$new()
  y <- mod$run(0:10)[, "y"]
  expect_equal(y, seq(1, by = 0.5, length.out = 11))
})


## issue #88
test_that("force a vector of strings (parse)", {
  ir <- odin_parse(c("deriv(y) <- 0.5", "initial(y) <- 1"))
  dat <- ir_deserialise(ir)
  expect_equal(names(dat$data$variable$contents), "y")
})


test_that("force a symbol containing code", {
  code <- c("deriv(y) <- 0.5", "initial(y) <- 1")
  gen <- odin(code, target = "r")
  mod <- gen$new()
  y <- mod$run(0:10)[, "y"]
  expect_equal(y, seq(1, by = 0.5, length.out = 11))
})


test_that("force a symbol containing code (parse)", {
  code <- c("deriv(y) <- 0.5", "initial(y) <- 1")
  ir <- odin_parse(code)
  dat <- ir_deserialise(ir)
  expect_equal(names(dat$data$variable$contents), "y")
})


test_that("odin_ir requires sensible object", {
  expect_error(odin_ir(NULL), "Expected an odin_generator or odin_model object")
})


## https://github.com/mrc-ide/odin/issues/154
test_that("delay discrete models with defaults are prevented in C", {
  skip_for_compilation()
  expect_error(odin({
    r <- 3.6
    update(y) <- r * y * (1 - y)
    initial(y) <- 0.2
    x <- delay(y, 2, 1)
    output(x) <- TRUE
  }, target = "c"),
  "Discrete delays with default not yet supported")

  expect_error(odin({
    r <- 3.6
    update(y[]) <- r * y[i] * (1 - y[i])
    initial(y[1]) <- 0.2
    initial(y[2]) <- 0.4
    x[] <- delay(y[i], 2, z[i])
    z[] <- user()
    output(x[]) <- TRUE
    dim(y) <- 2
    dim(x) <- 2
    dim(z) <- 2
  }),
  "Discrete delays with default not yet supported")
})


test_that("Allow spaces in filenames", {
  skip_on_cran()
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  filename <- file.path(path, "path with spaces.R")
  writeLines(c("initial(x) <- 1", "deriv(x) <- 1"), filename)

  mod <- odin(filename, target = "c")
  expect_equal(ir_deserialise(odin_ir(mod))$config$base,
               "path_with_spaces")
})


test_that("compatibility layer passes to R6 class", {
  gen <- odin(c("deriv(y) <- 0.5", "initial(y) <- 1"), target = "r")
  gen_r6 <- attr(gen, "generator")

  expect_equal(capture.output(print(gen)), capture.output(print(gen_r6)))
  expect_equal(utils::.DollarNames(gen), utils::.DollarNames(gen_r6))
  expect_identical(gen$classname, gen_r6$classname)
  expect_identical(gen[["classname"]], gen_r6[["classname"]])
})


unload_dlls()
