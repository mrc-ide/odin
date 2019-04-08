context("model cache")

test_that("model_cache", {
  obj <- R6_ring_cache$new(10)

  expect_equal(obj$list(), character())
  expect_null(obj$get("a"))
  x <- runif(10)
  obj$put("a", x)
  expect_equal(obj$list(), "a")
  expect_equal(obj$get("a"), x)

  ## overflow the ring
  obj$resize(4)
  for (x in letters[2:4]) {
    obj$put(x, x)
  }
  expect_equal(obj$list(), c("d", "c", "b", "a"))

  obj$put("e", "e")
  expect_equal(obj$list(), c("e", "d", "c", "b"))

  obj$put("c", "c")
  expect_equal(obj$list(), c("c", "e", "d", "b"))

  obj$get("d")
  expect_equal(obj$list(), c("d", "c", "e", "b"))

  obj$resize(2)
  expect_equal(obj$list(), c("d", "c"))

  obj$clear()
  expect_equal(obj$list(), character())
})


test_that("reused cached model", {
  model_cache_clear()
  code <- c("deriv(y) <- 0.5",
            "initial(y) <- 1")

  gen <- odin(code)
  expect_equal(.odin$model_cache_c$list(),
               hash_string(gen()$ir))
  expect_message(odin(code, verbose = TRUE),
                 "Using cached model")
  expect_silent(odin(code, verbose = FALSE))
})


test_that("reuse shared library", {
  model_cache_clear()
  code <- c("deriv(y) <- 0.5",
            "initial(y) <- 1")

  path <- tempfile()
  dir.create(path)
  gen <- odin(code, skip_cache = TRUE, workdir = path)
  expect_message(
    odin(code, verbose = TRUE, skip_cache = TRUE, workdir = path),
    "Using previously compiled shared library")
  expect_equal(length(dir(path, pattern = paste0(.Platform$dynlib.ext, "$"))),
               1)
})

unload_dlls()
