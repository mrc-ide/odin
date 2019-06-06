context("preprocess")

test_that("text", {
  src <- c("deriv(y) <- 0.5",
           "initial(y) <- 1")
  f <- function(x) lapply(x$exprs, identity)
  cmp <- lapply(parse(text = src), identity)

  ## Accept the source as a string:
  expect_equal(odin_preprocess_detect(src), "text")
  expect_equal(f(odin_preprocess(src)), cmp)
  expect_equal(odin_preprocess_detect(paste(src, collapse = "\n")), "text")
  expect_equal(odin_preprocess_detect(paste(src, collapse = ";")), "text")
  expect_equal(f(odin_preprocess(paste(src, collapse = "\n"))), cmp)
  expect_equal(f(odin_preprocess(paste(src, collapse = ";"))), cmp)

  dest <- tempfile()
  writeLines(src, dest)
  expect_equal(odin_preprocess_detect(dest), "file")
  expect_equal(f(odin_preprocess(dest)), cmp)

  expect_error(odin_preprocess(tempfile()),
               "looks like a filename, but file does not exist")

  expect_error(odin_preprocess(1L), "Invalid type")
  expect_error(odin_preprocess(pi), "Invalid type")
  expect_error(odin_preprocess(sin), "Invalid type")
  expect_error(odin_preprocess(1.0), "Invalid type")
})


test_that("type detection avoids unlikely filenames", {
  expect_error(odin_preprocess_detect("x"), "looks like a filename")
  expect_equal(odin_preprocess_detect("x <- y"), "text")
  expect_equal(odin_preprocess_detect("x = y"), "text")
  expect_equal(odin_preprocess_detect("deriv(x)"), "text")
})


test_that("type detection can skip filenames", {
  expect_error(odin_preprocess_detect("x", NULL), "looks like a filename")
  expect_equal(odin_preprocess_detect("x", "text"), "text")
  expect_error(odin_preprocess_detect("x", "file"), "does not exist")
})


test_that("detect invalid type", {
  expect_error(odin_preprocess_detect("x", "expression"),
               "Invalid input for odin - expected expression")
  expect_error(odin_preprocess_detect(quote(x), "text"),
               "Invalid input for odin - expected text")
  expect_error(odin_preprocess_detect(quote(x), "file"),
               "Invalid input for odin - expected file")
})


test_that("handle empty input", {
  ## Previously errored
  expect_equal(odin_preprocess_detect(character(0)), "text")
})
