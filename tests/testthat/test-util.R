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

test_that("parse gcc warnings", {
  ret <- classify_compiler_output(readLines("logs/gcc_warnings.txt"))
  expect_equal(ret$type,
               c("command", "context", "info", "context", "info", "command"))
  expect_equal(attr(ret$value[[3]], "type"), "warning")
  expect_equal(attr(ret$value[[5]], "type"), "warning")
})
