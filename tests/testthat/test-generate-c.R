context("generate: c (tools)")

test_that("Integer sums over arrays not supported", {
  data <- list(elements = list(y = list(storage_type = "int")))
  expect_error(
    generate_c_sexp(list("sum", "y", 1, 2), data, list(), character()),
    "Partial integer sums not yet supported")
  data$elements$y$storage_type <- "double"
  expect_equal(
    generate_c_sexp(list("sum", "y", 1, 2), data, list(), character()),
    "odin_sum1(y, 0, 2)")
})
