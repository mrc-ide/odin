context("utilities")

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


test_that("odin_version", {
  expect_true(setequal(names(odin_version()),
                       c("odin", "cinterpolate", "r", "platform")))
})


test_that("match_value", {
  object <- "foo"
  expect_error(match_value(object, letters), "object must be one of {a, ",
               fixed = TRUE)
  expect_silent(match_value("a", letters))
})


test_that("adrop works", {
  x <- 1:10
  m <- array(x, c(1, 10, 1))
  expect_equal(adrop(m, 1), cbind(x, deparse.level = 0))
  expect_error(adrop(m, 2), "Can't drop selected dimensions")
  expect_equal(adrop(m, 3), rbind(x, deparse.level = 0))

  expect_equal(adrop(array(x, c(1, 2, 5)), 1),
               matrix(x, 2, 5))
  expect_equal(adrop(array(x, c(2, 1, 5)), 2),
               matrix(x, 2, 5))
  expect_equal(adrop(array(x, c(2, 5, 1)), 3),
               matrix(x, 2, 5))
})


test_that("sprintf_safe throws on empty arguments", {
  expect_error(sprintf_safe("%s", NULL),
               "Passed empty format parameter to formatter")
  expect_error(sprintf_safe("%s %s", 1, NULL),
               "Passed empty format parameter to formatter")
  expect_error(sprintf_safe("%s %s", NULL, 1),
               "Passed empty format parameter to formatter")
  expect_equal(sprintf_safe("%s %s", "a", "b"),
               sprintf("%s %s", "a", "b"))
})
