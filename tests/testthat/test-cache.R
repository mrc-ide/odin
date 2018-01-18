context("model cache")

test_that("invalidate cache", {
  model_cache_clear()
  code <- c("deriv(y) <- 0.5",
            "initial(y) <- 1")

  gen <- odin(code, verbose = TEST_VERBOSE)
  expect_equal(model_cache_list(), hash_model(code))
  expect_message(odin(code, verbose = TRUE),
                 "Using cached model")

  dat <- model_cache_get(hash_model(code))

  expect_true(is_directory(dat$dll$path))
  expect_true(file.exists(dat$dll$dll))

  model_cache_clear()
  expect_message(
    gen <- odin(code, dest = dat$dll$path, verbose = TRUE),
    "Using previously compiled shared library")
})

test_that("includes", {
  fns <- readLines("user_fns.c")
  fns2 <- gsub("t1", "t2", fns)
  user <- basename(tempfile("odin_", ".", ".c"))
  writeLines(fns, user)
  on.exit(file.remove(user))
  expr <- substitute_(quote({
    config(include) <- user_fns
    z <- squarepulse(t, 1, 2)
    output(z) <- z
    deriv(y) <- z
    initial(y) <- 0
  }), list(user_fns = user))

  gen <- odin(expr, verbose = TEST_VERBOSE)
  expect_message(odin(expr, verbose = TRUE), "Using cached model")

  writeLines(fns2, user)
  expect_message(capture.output(gen <- odin(expr, verbose = TRUE)),
                 "Compiling shared library")
  expect_message(gen <- odin(expr, verbose = TRUE),
                 "Using cached model")
})

unload_dlls()
