context("interface")

test_that("NSE and SE defaults are the same", {
  expect_equal(formals(odin), formals(odin_))
})

test_that("verbose", {
  expect_output(
    odin({
      initial(x) <- 0
      update(x) <- x + norm_rand()
      config(base) <- "mycrazymodel"
    }, target = "c", workdir = tempfile(), skip_cache = TRUE, verbose = TRUE),
    "mycrazymodel_[[:xdigit:]]{8}\\.c")
})

test_that("warnings", {
  skip_on_cran() # this test is platform specific!
  code <- quote({
    initial(a) <- 1
    deriv(a) <- if (t > 8 || t > 1 && t < 3) 1 else 0
  })

  str <- capture.output(
    tmp <- odin_(code, verbose = TRUE, compiler_warnings = FALSE,
                 skip_cache = TRUE, workdir = tempfile()))
  out <- compiler_output_classify(str)

  ## This will only give a warning with -Wall or greater.
  has_warning <- any(vlapply(seq_along(out$type), function(i)
    out$type[i] == "info" && attr(out$value[[i]], "type") == "warning"))
  if (has_warning) {
    re <- "(There was 1 compiler warning|There were [0-9]+ compiler warnings)"
    expect_warning(
      odin_(code, compiler_warnings = TRUE, skip_cache = TRUE,
            workdir = tempfile()),
      re)

    with_options(
      list(odin.compiler_warnings = FALSE),
      expect_warning(odin_(code, verbose = FALSE, skip_cache = TRUE,
                           workdir = tempfile()), NA))
    with_options(
      list(odin.compiler_warnings = TRUE),
      expect_warning(odin_(code, verbose = FALSE, skip_cache = TRUE,
                           workdir = tempfile()), re))
  } else {
    expect_warning(odin_(code, compiler_warnings = TRUE, verbose = FALSE,
                         skip_cache = TRUE, workdir = tempfile()), NA) # none
  }
})

test_that("n_history is configurable", {
  gen <- odin({
    ylag <- delay(y, 10)
    initial(y) <- 0.5
    deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
  })

  mod <- gen(use_dde = TRUE)
  expect_true("n_history" %in% names(formals(mod$run)))
  expect_error(mod$run(seq(0, 200), n_history = 0),
               "Integration failure: can't use ylag in model with no history")

  mod <- gen(use_dde = FALSE)
  expect_true("n_history" %in% names(formals(mod$run)))
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
  mod <- gen()
  y <- mod$run(0:10)[, "y"]
  expect_equal(y, seq(1, by = 0.5, length.out = 11))
})


## issue #88
test_that("force a vector of strings (parse)", {
  ir <- odin_parse(c("deriv(y) <- 0.5", "initial(y) <- 1"))
  dat <- ir_deserialise(ir)
  expect_equal(names(dat$data$variable$contents), "y")
})


test_that("odin_ir requires sensible object", {
  expect_error(odin_ir(NULL), "Expected an odin_generator or odin_model object")
})


## https://github.com/mrc-ide/odin/issues/154
test_that("delay discrete models with defaults are prevented in C", {
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


unload_dlls()
