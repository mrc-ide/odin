context("compile")

test_that("compilation failure", {
  path <- tempfile(fileext = ".c")
  writeLines("this is a test", path)
  expect_error(compile(path),
               "Error compiling source")
})


test_that("compilation failure", {
  str <- readLines("logs/gcc_error.txt")
  ret <- compiler_output_classify(str)
  expect_equal(ret$type,
               c("command", "info", "info", "context", "info", "info", "info",
                 "info", "context", "info", "info", "command"))
  expect_equal(vcapply(ret$value[ret$type == "info"], attr, "type"),
               c("error", "error", "warning", "error",
                 "note", "error", "error", "error"))
})


test_that("unclassifiable output", {
  txt <- readLines("logs/gcc_warnings.txt")
  cmp <- compiler_output_classify(txt)
  extra <- "here's another string"
  res <- compiler_output_classify(c(txt, extra))

  expect_equal(res$type, c(cmp$type, "unknown"))
  expect_equal(res$value, c(cmp$value, extra))
  format(res)
})


test_that("parse gcc warnings", {
  ret <- compiler_output_classify(readLines("logs/gcc_warnings.txt"))
  expect_equal(ret$type,
               c("command", "context", "info", "context", "info", "command"))
  expect_equal(attr(ret$value[[3]], "type"), "warning")
  expect_equal(attr(ret$value[[5]], "type"), "warning")
  format(ret)
})


test_that("empty compiler output", {
  expect_equal(compiler_output_classify(character(0)),
               structure(list(type = character(0), value = list()),
                         class = "compiler_output"))
  expect_equal(format(compiler_output_classify(character(0))), "")
})


test_that("compilation warning", {
  expect_warning(
    compiler_output_handle(readLines("logs/gcc_warnings.txt"), TRUE, TRUE),
    "There were 2 compiler warnings")
})
