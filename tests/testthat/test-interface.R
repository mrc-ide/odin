context("interface")

test_that("NSE and SE defaults are the same", {
  expect_equal(formals(odin2), formals(odin2_))
})

test_that("verbose", {
  skip("verbose")
  ## needs writing the temporary file with a better name
  expect_output(odin2({
    initial(x) <- 0
    update(x) <- x + norm_rand()
    config(base) <- "mycrazymodel"
  }, target = "c", verbose = TRUE),
  "mycrazymodel.o", fixed = TRUE)
})

test_that("warnings", {
  code <- quote({
    initial(a) <- 1
    deriv(a) <- if (t > 8 || t > 1 && t < 3) 1 else 0
  })

  str <- capture.output(
    tmp <- odin2_(code, verbose = TRUE, compiler_warnings = FALSE))
  out <- classify_compiler_output(str)

  ## This will only give a warning with -Wall or greater.
  has_warning <- any(vlapply(seq_along(out$type), function(i)
    out$type[i] == "info" && attr(out$value[[i]], "type") == "warning"))
  if (has_warning) {
    re <- "(There was 1 compiler warning|There were [0-9]+ compiler warnings)"
    expect_warning(odin2_(code, compiler_warnings = TRUE), re)

    with_options(
      list(odin.compiler_warnings = FALSE),
      expect_warning(odin2_(code, verbose = FALSE), NA))

    with_options(
      list(odin.compiler_warnings = TRUE),
      expect_warning(odin2_(code, verbose = FALSE), re))
  } else {
    expect_warning(odin2_(code, compiler_warnings = TRUE,
                               verbose = FALSE), NA) # none
  }
})

test_that("n_history is configurable", {
  gen <- odin2({
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
  skip("pending")
  path <- tempfile()
  writeLines("", path)
  expect_error(odin2_(path), "Empty input: no expressions were provided")
  writeLines("# some comment", path)
  expect_error(odin2_(path), "Empty input: no expressions were provided")
})


test_that("prevent unknown target", {
  expect_error(odin2({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2
  }, target = "fortran"),
  "Unknown target 'fortran'", fixed = TRUE)
})


## issue #88
test_that("force a vector of strings (compile)", {
  gen <- odin2(c("deriv(y) <- 0.5", "initial(y) <- 1"), target = "r")
  mod <- gen()
  y <- mod$run(0:10)[, "y"]
  expect_equal(y, seq(1, by = 0.5, length.out = 11))
})


## issue #88
test_that("force a vector of strings (parse)", {
  ir <- odin_parse2(c("deriv(y) <- 0.5", "initial(y) <- 1"))
  dat <- ir_deserialise(ir)
  expect_equal(names(dat$data$variable$contents), "y")
})


unload_dlls()
