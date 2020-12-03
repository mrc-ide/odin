context("odin_options")

test_that("can create placeholder handler for include parsing", {
  opts <- odin_options(target = "fortran")
  expect_error(
    opts$read_include(),
    "'config(include)' is not supported for target 'fortran'", fixed = TRUE)
})
