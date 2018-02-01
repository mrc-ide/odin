context("utilities")

test_that("can_compile", {
  expect_true(can_compile())
  expect_true(can_compile())
  expect_true(can_compile(skip_cache = TRUE))
})

test_that("read_user_c - simple", {
  ans <- read_user_c("user_fns.c")
  expect_equal(names(ans$declarations), "squarepulse")
  expect_equal(names(ans$definitions), "squarepulse")
})

test_that("read_user_c - split declaration", {
  ans <- read_user_c("user_fns2.c")
  expect_equal(names(ans$declarations), "squarepulse")
  expect_equal(names(ans$definitions), "squarepulse")
  expect_match(ans$declarations, "double t1\\);$")
})

test_that("read_user_c - parse error", {
  expect_error(read_user_c("user_fns3.c"),
               "Parse error for user_fns3.c", fixed = TRUE)
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

test_that("unclassifiable output", {
  txt <- readLines("logs/gcc_warnings.txt")
  cmp <- classify_compiler_output(txt)
  extra <- "here's another string"
  res <- classify_compiler_output(c(txt, extra))

  expect_equal(res$type, c(cmp$type, "unknown"))
  expect_equal(res$value, c(cmp$value, extra))
  format(res)
})

test_that("symbol_sum", {
  expect_equal(symbol_sum(list(1)), 1)
  expect_equal(symbol_sum(list("a")), quote(a))

  expect_equal(symbol_sum(list(1, "a")), quote(1 + a))
  expect_equal(symbol_sum(list("a", "b")), quote(a + b))

  expect_equal(symbol_sum(list(1, "a", "b")), quote(1 + a + b))
  expect_equal(symbol_sum(list("a", "b", "c")), quote(a + b + c))
})

test_that("hash_files", {
  expect_error(hash_files(tempfile()), "Files missing")
})

test_that("odin_version", {
  expect_identical(odin_version(), ODIN_VERSION)
  expect_true(setequal(names(ODIN_VERSION),
                       c("odin", "cinterpolate", "r", "platform")))
})
