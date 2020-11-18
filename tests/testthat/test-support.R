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


test_that("multivariate hypergeometric distribution", {
  k <- c(6, 10, 15, 3, 0, 4)
  n <- 20
  N <- sum(k)

  set.seed(1)
  res <- t(replicate(5000, rmhyper(n, k)))

  ## Population is preserved
  expect_true(all(rowSums(res) == n))

  ## Mean
  expect_equal(colMeans(res), n * k / N, tolerance = 0.05)

  ## Variance and covariance
  expected <- outer(k, k, function(ki, kj)
    - n * (N - n) / (N - 1) * ki / N * kj / N)
  diag(expected) <- n * (N - n) / (N - 1) * k / N * (1 - k / N)
  expect_equal(cov(res), expected, tolerance = 0.05)
})


test_that("prevent oversampling in rmhyper", {
  expect_error(
    rmhyper(100, 0:5),
    "Requesting too many elements in rmhyper (100 from 15)",
    fixed = TRUE)
})


test_that("multivariate hypergeometric distribution (C)", {
  skip_on_cran()

  lib <- read_user_c(system.file("library.c", package = "odin"))
  impl <- unname(lib$definitions[c("rmhyper", "rmhyper_i", "rmhyper_d")])
  code <- c(
    "#include <R.h>",
    "#include <Rmath.h>",
    "#include <Rinternals.h>",
    impl,
    "SEXP test_rmhyper(SEXP n, SEXP k) {",
    "  size_t m = (size_t) length(k);",
    "  SEXP ret = PROTECT(allocVector(INTSXP, m));",
    "  GetRNGstate();",
    "  if (TYPEOF(k) == INTSXP) {",
    "    rmhyper_i(INTEGER(n)[0], INTEGER(k), m, INTEGER(ret));",
    "  } else {",
    "    rmhyper_d(INTEGER(n)[0], REAL(k), m, INTEGER(ret));",
    "  }",
    "  PutRNGstate();",
    "  UNPROTECT(1);",
    "  return ret;",
    "}")

  path <- tempfile()
  path_src <- file.path(path, "src")
  dir.create(path_src, FALSE, TRUE)
  name <- "test.rmhyper"
  writeLines(code, file.path(path_src, "rmhyper.c"))
  for (f in c("DESCRIPTION", "NAMESPACE")) {
    writeLines(sprintf(readLines(file.path("pkg", f)), name),
               file.path(path, f))
  }

  ## NOTE: could either use pkgload or pkgbuild here, not clear which
  ## is better.
  dll <- file.path(path_src,
                   basename(pkgbuild::compile_dll(path, quiet = TRUE)))

  dyn.load(dll)
  on.exit(dyn.unload(dll))

  rmhyper_c <- function(n, k) {
    .Call("test_rmhyper", as.integer(n), k, PACKAGE = name)
  }

  k <- c(6, 10, 15, 3, 0, 4)
  n <- 20
  set.seed(1)
  a <- replicate(500, rmhyper(n, k))
  set.seed(1)
  b1 <- replicate(500, rmhyper_c(n, as.integer(k)))
  set.seed(1)
  b2 <- replicate(500, rmhyper_c(n, as.double(k)))

  expect_equal(b1, a)
  expect_equal(b2, a)
  expect_identical(b1, b2)

  expect_error(
    rmhyper_c(100, as.integer(k)),
    "Requesting too many elements in rmhyper (100 from 38)",
    fixed = TRUE)
})
