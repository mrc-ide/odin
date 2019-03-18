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


test_that("onload can be rerun safely", {
  expect_silent(.onLoad())
})


test_that("find package root", {
  path <- tempfile()
  p1 <- file.path(path, "odin/a.json")
  p2 <- file.path(path, "inst/odin/a.json")
  p3 <- file.path(path, "inst/odin/b.json")

  dir.create(dirname(p1), FALSE, TRUE)
  dir.create(dirname(p2), FALSE, TRUE)
  file.create(p1)
  file.create(p2)
  file.create(p3)

  expect_equal(package_odin_path("odin/a.json", NULL, path), p2)
  expect_equal(package_odin_path("odin/b.json", NULL, path), p3)

  unlink(p2)
  expect_equal(package_odin_path("odin/a.json", NULL, path), p1)
  unlink(p1)
  expect_error(package_odin_path("odin/a.json", NULL, path),
               "Could not find odin ir")
})
