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

unload_dlls()
