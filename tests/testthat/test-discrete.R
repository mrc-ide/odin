context("discrete")

test_that("basic", {
  gen <- odin::odin({
    initial(x) <- 1
    update(x) <- x + 1
    config(base) <- "discrete"
  }, verbose = TEST_VERBOSE)

  mod <- gen()
  expect_equal(mod$contents(), list(initial_x = 1))
  y0 <- mod$initial()
  expect_equal(y0, 1.0)
  expect_equal(mod$update(0L, y0), 2.0)

  tt <- 0:10
  res <- mod$run(tt)

  expect_equal(res, cbind(step = tt, x = 1:11))
})
