context("odin_build")


test_that("build from validate", {
  model_cache_clear()
  options <- odin_options(verbose = TRUE, workdir = tempfile())
  code <- c("initial(x) <- 1", "deriv(x) <- 1")
  x <- odin_validate(code, "text")
  res <- odin_build(x, options)
  expect_is(res$model, "odin_generator")
  expect_is(res$output, "character")
  expect_identical(res$ir, x$result)
  expect_true(res$success)
  expect_null(res$error)
  expect_is(res$elapsed, "proc_time")
})


test_that("build from ir", {
  model_cache_clear()
  options <- odin_options(verbose = TRUE, workdir = tempfile())
  code <- c("initial(x) <- 1", "deriv(x) <- 1")
  x <- odin_parse_(code, type = "text")
  res <- odin_build(x, options)
  expect_is(res$model, "odin_generator")
  expect_is(res$output, "character")
  expect_identical(res$ir, x)
  expect_true(res$success)
  expect_null(res$error)
  expect_is(res$elapsed, "proc_time")
})


test_that("build failure", {
  code <- c("initial(x) <- 1", "deriv(x) <- 1")
  x <- odin_parse_(code, type = "text")
  res <- odin_build(substr(x, 2, nchar(x)))
  expect_null(res$model)
  expect_is(res$error, "character")
  expect_false(res$success)
})


test_that("invalid input", {
  code <- c("initial(x) <- 1", "deriv(x) <- 1")
  x <- odin_parse_(code, type = "text")
  expect_error(odin_build(as.character(x)),
               "Expected an odin intermediate representation")
})
