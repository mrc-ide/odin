test_that("can create functions", {
  expect_equal(
    js_function(c("a"), "return 1;"),
    c("function(a) {", "  return 1;", "}"))
  expect_equal(
    js_function(c("a"), "return 1;", "constructor"),
    c("constructor(a) {", "  return 1;", "}"))
  expect_equal(
    js_function(c("a"), "return 1;", "name"),
    c("function name(a) {", "  return 1;", "}"))
})
