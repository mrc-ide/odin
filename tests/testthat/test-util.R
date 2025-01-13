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


test_that("collector works", {
  obj <- collector()
  expect_equal(obj$get(), character(0))
  expect_equal(obj$length(), 0L)
  obj$add("a")
  expect_equal(obj$get(), "a")
  expect_equal(obj$length(), 1L)
  obj$add(c("b", "c"))
  expect_equal(obj$get(), c("a", "b", "c"))
  expect_equal(obj$length(), 3L)
})


test_that("collector_list works", {
  obj <- collector_list()
  expect_equal(obj$get(), list())
  obj$add("a")
  expect_equal(obj$get(), list("a"))
  obj$add(c("b", "c"))
  expect_equal(obj$get(), list("a", c("b", "c")))
})


test_that("counter works", {
  obj <- counter()
  expect_equal(obj$get(), 0L)
  obj$add()
  expect_equal(obj$get(), 1L)
  obj$add()
  expect_equal(obj$get(), 2L)
  obj$reset()
  expect_equal(obj$get(), 0L)
})


test_that("Can avoid debug in compile_dll", {
  skip_if_not_installed("mockery")
  skip_on_cran()

  mock_has_user_makevars <- mockery::mock(FALSE)
  mock_compile_dll <- mockery::mock(
    list(Sys.getenv("R_MAKEVARS_USER"), pkgbuild:::makevars_user()))

  path <- tempfile()
  compile_attributes <- TRUE
  quiet <- FALSE
  mockery::stub(compile_dll, "has_user_makevars", mock_has_user_makevars)
  mockery::stub(compile_dll, "pkgbuild::compile_dll", mock_compile_dll)
  res <- compile_dll(path, compile_attributes, quiet)

  expect_equal(res[[1]], res[[2]])
  expect_equal(normalizePath(dirname(res[[1]])),
               normalizePath(tempdir()))

  mockery::expect_called(mock_has_user_makevars, 1)
  mockery::expect_called(mock_compile_dll, 1)
  expect_equal(
    mockery::mock_args(mock_compile_dll)[[1]],
    list(path, compile_attributes, quiet))
})


test_that("Don't set envvar if not needed", {
  skip_if_not_installed("mockery")
  skip_on_cran()

  env <- c("R_MAKEVARS_USER" = NA)
  cmp <- withr::with_envvar(
    env,
    pkgbuild:::makevars_user())

  mock_has_user_makevars <- mockery::mock(TRUE)
  mock_compile_dll <- mockery::mock(
    list(Sys.getenv("R_MAKEVARS_USER"), pkgbuild:::makevars_user()))

  path <- tempfile()
  compile_attributes <- TRUE
  quiet <- FALSE

  mockery::stub(compile_dll, "has_user_makevars", mock_has_user_makevars)
  mockery::stub(compile_dll, "pkgbuild::compile_dll", mock_compile_dll)

  res <- withr::with_envvar(
    env,
    compile_dll(path, compile_attributes, quiet))

  expect_equal(res[[1]], "")
  expect_equal(res[[2]], cmp)

  mockery::expect_called(mock_has_user_makevars, 1)
  mockery::expect_called(mock_compile_dll, 1)
  expect_equal(
    mockery::mock_args(mock_compile_dll)[[1]],
    list(path, compile_attributes, quiet))
})


test_that("validate inputs", {
  expect_silent(assert_scalar_logical_or_null(NULL))
  expect_silent(assert_scalar_logical_or_null(TRUE))
  expect_silent(assert_scalar_logical_or_null(FALSE))

  thing <- "true"
  expect_error(
    assert_scalar_logical_or_null(thing),
    "Expected 'thing' to be a logical scalar (or NULL)",
    fixed = TRUE)
  expect_error(assert_scalar_logical_or_null(NA),
               "Expected '.+' to be a logical scalar \\(or NULL\\)")
  expect_error(assert_scalar_logical_or_null(logical(0)),
               "Expected '.+' to be a logical scalar \\(or NULL\\)")
})


test_that("validate inputs", {
  expect_silent(assert_scalar_character_or_null(NULL))
  expect_silent(assert_scalar_character_or_null("a"))

  thing <- TRUE
  expect_error(
    assert_scalar_character_or_null(thing),
    "Expected 'thing' to be a character scalar (or NULL)",
    fixed = TRUE)
  expect_error(assert_scalar_character_or_null(NA),
               "Expected '.+' to be a character scalar \\(or NULL\\)")
  expect_error(assert_scalar_character_or_null(character(0)),
               "Expected '.+' to be a character scalar \\(or NULL\\)")
})


test_that("check names", {
  expect_error(
    assert_named(list()),
    "must be named")
  expect_error(
    assert_named(list(1, 2)),
    "must be named")
  expect_silent(
    assert_named(list(a = 1, a = 2)))
  expect_error(
    assert_named(list(a = 1, a = 2), TRUE),
    "must have unique names")
})


test_that("Check S3 class", {
  expect_silent(assert_is(structure(1, class = "foo"), "foo"))
  expect_error(assert_is(structure(1, class = "bar"), "foo"),
               "must be a foo")
  expect_error(assert_is(1, c("foo", "bar")),
               "must be a foo / bar")
})
