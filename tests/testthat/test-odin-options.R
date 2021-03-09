context("odin_options")

test_that("odin_options creates a classed list", {
  opts <- odin_options()
  expect_s3_class(opts, "odin_options")
  expect_true(is.list(opts))
})

test_that("can create placeholder handler for include parsing", {
  opts <- odin_options(target = "fortran")
  expect_error(
    opts$read_include(),
    "'config(include)' is not supported for target 'fortran'", fixed = TRUE)
})


test_that("manually set parsing functions persist", {
  opts <- odin_options(target = "fortran")
  opts$read_include <- read_include_c
  expect_identical(odin_options(options = opts)$read_include, read_include_c)
})
