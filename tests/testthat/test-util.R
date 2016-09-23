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
  format(ret)
})

test_that("empty compiler output", {
  expect_equal(classify_compiler_output(character(0)),
               structure(list(type = character(0), value = list()),
                         class = "compiler_output"))
  expect_equal(format(classify_compiler_output(character(0))), "")
})

test_that("compilation warning", {
  expect_warning(
    handle_compiler_output(readLines("logs/gcc_warnings.txt"), TRUE, TRUE),
    "There were 2 compiler warnings")
})

test_that("compilation failure", {
  path <- tempfile(fileext = ".c")
  writeLines("this is a test", path)
  expect_error(compile(path),
               "Error compiling source")
})

test_that("compilation failure", {
  str <- readLines("logs/gcc_error.txt")
  ret <- classify_compiler_output(str)
  expect_equal(ret$type,
               c("command", "info", "info", "context", "info", "info", "info",
                 "info", "context", "info", "info", "command"))
  expect_equal(vcapply(ret$value[ret$type == "info"], attr, "type"),
               c("error", "error", "warning", "error",
                 "note", "error", "error", "error"))
})
