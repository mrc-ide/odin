context("interface")

test_that("text", {
  src <- c("deriv(y) <- 0.5",
           "initial(y) <- 1")
  f <- function(x) lapply(x, identity)
  cmp <- f(parse(text=src))

  ## Accept the source as a string:
  expect_equal(odin_parse_prepare_detect(src), "text")
  expect_equal(f(odin_parse_prepare(src)), cmp)
  expect_equal(odin_parse_prepare_detect(paste(src, collapse="\n")), "text")
  expect_equal(odin_parse_prepare_detect(paste(src, collapse=";")), "text")
  expect_equal(f(odin_parse_prepare(paste(src, collapse="\n"))), cmp)
  expect_equal(f(odin_parse_prepare(paste(src, collapse=";"))), cmp)

  dest <- tempfile()
  writeLines(src, dest)
  expect_equal(odin_parse_prepare_detect(dest), "file")
  expect_equal(f(odin_parse_prepare(dest)), cmp)

  expect_error(odin_parse_prepare(tempfile()),
               "looks like a filename, but file does not exist")

  expect_error(odin_parse_prepare(1L), "Invalid type")
  expect_error(odin_parse_prepare(pi), "Invalid type")
  expect_error(odin_parse_prepare(sin), "Invalid type")
  expect_error(odin_parse_prepare(1.0), "Invalid type")
})

test_that("NSE and SE defaults are the same", {
  expect_equal(formals(odin), formals(odin_))
})

test_that("Missing directory", {
  expect_error(
    odin::odin({
      deriv(y) <- 0.5
      initial(y) <- 1
    }, tempfile()),
    "'dest' must be an existing directory")
})

test_that("verbose", {
  expect_output(odin::odin({
    initial(x) <- 0
    update(x) <- x + norm_rand()
    config(base) <- "mycrazymodel"
  }, verbose = TRUE),
  "mycrazymodel.o", fixed=TRUE)
})

test_that("warnings", {
  code <- quote({
    initial(a) <- 1
    deriv(a) <- if (t > 8 || t > 1 && t < 3) 1 else 0
  })

  str <- capture.output(
    tmp <- odin::odin_(code, verbose = TRUE, compiler_warnings = FALSE))
  out <- classify_compiler_output(str)
  ## This will only give a warning with -Wall or greater.
  has_warning <- any(vlapply(seq_along(out$type), function(i)
    out$type[i] == "info" && attr(out$value[[i]], "type") == "warning"))
  if (has_warning) {
    re <- "(There was 1 compiler warning|There were [0-9]+ compiler warnings)"
    expect_warning(odin::odin_(code, compiler_warnings = TRUE), re)

    oo <- options(odin.compiler_warnings = FALSE)
    on.exit(options(oo))

    expect_warning(odin::odin_(code, verbose = FALSE), NA)
    options(odin.compiler_warnings = TRUE)
    expect_warning(odin::odin_(code, verbose = FALSE), re)
  } else {
    expect_warning(odin::odin_(code, compiler_warnings = TRUE,
                               verbose = FALSE), NA) # none
  }
})

test_that("n_history is configurable", {
  gen <- odin::odin({
    ylag <- delay(y, 10)
    initial(y) <- 0.5
    deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
  }, verbose=TEST_VERBOSE)

  mod <- gen(use_dde = TRUE)
  expect_true("n_history" %in% names(formals(mod$run)))
  expect_error(mod$run(seq(0, 200), n_history = 0),
               "Integration failure: did not find time in history")

  mod <- gen(use_dde = FALSE)
  expect_true("n_history" %in% names(formals(mod$run)))
  ## Don't test for precice deSolve error message; just test fail/pass
  expect_error(mod$run(seq(0, 200), n_history = 1))
  expect_error(mod$run(seq(0, 200), n_history = 1000), NA)
})

unload_dlls()
