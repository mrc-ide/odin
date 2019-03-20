context("support")

test_that("user parameters must be named", {
  expect_silent(support_check_user(NULL, NULL))
  expect_error(support_check_user(1, NULL),
               "All user parameters must be named")
  expect_error(support_check_user(list(1), NULL),
               "All user parameters must be named")
  expect_error(support_check_user(list(a = 1, 2), NULL),
               "All user parameters must be named")
})


test_that("duplicate user parameters are not allowed", {
  expect_error(support_check_user(list(a = 1, a = 2), NULL),
               "Duplicated user parameters: a")
  expect_error(support_check_user(list(a = 1, a = 2, b = 1), NULL),
               "Duplicated user parameters: a")
  expect_error(support_check_user(list(a = 1, a = 2, b = 1, b = 2), NULL),
               "Duplicated user parameters: a, b")
  expect_error(
    support_check_user(list(a = 1, a = 2, a = 3, b = 1, b = 2), NULL),
    "Duplicated user parameters: a, b")
})


test_that("unknown user parameters are not allowed", {
  expect_error(support_check_user(list(a = 1), "b", "stop"),
               "Unknown user parameters: a")
  expect_error(support_check_user(list(a = 1, b = 2), "b", "stop"),
               "Unknown user parameters: a")
  expect_error(support_check_user(list(a = 1, b = 2, c = 3), "b", "stop"),
               "Unknown user parameters: a, c")
})


test_that("as_integer converts appropriately", {
  expect_identical(as_integer(1.0), 1L)
  expect_identical(as_integer(1L), 1L)
  expect_identical(as_integer(1 + .Machine$double.eps), 1L)
  expect_identical(as_integer(c(1, 2)), c(1L, 2L))
})


test_that("as_integer errors appropriately", {
  expect_error(as_integer(pi), "Expected integer input for 'pi'")
  expect_error(as_integer(TRUE), "Expected integer input for 'TRUE'")
})


test_that("as_numeric converts appropriately", {
  expect_identical(as_numeric(1L), 1.0)
  expect_identical(as_numeric(1.0), 1.0)
  expect_identical(as_numeric(1 + .Machine$double.eps),
                   1 + .Machine$double.eps)
  expect_identical(as_numeric(c(1L, 2L)), c(1, 2))
})


test_that("as_numeric errors appropriately", {
  expect_error(as_numeric(TRUE), "Expected numeric input for 'TRUE'")
  expect_error(as_numeric("x"), "Expected numeric input for")
})
