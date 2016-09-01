context("utilities")

test_that("can_compile", {
  expect_true(can_compile())
  expect_true(can_compile())
  expect_true(can_compile(skip_cache=TRUE))
})

test_that("read_user_c - simple", {
  ans <- read_user_c("user_fns.c")
  expect_equal(names(ans$declarations), "squarepulse")
  expect_equal(names(ans$definitions), "squarepulse")
})

## This currently fails
test_that("read_user_c - split declaration", {
  expect_error(read_user_c("user_fns2.c"), "Parse error")
})
