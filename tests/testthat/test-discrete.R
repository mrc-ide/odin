context("discrete")

test_that("basic", {
  gen <- odin::odin({
    initial(x) <- 1
    update(x) <- x + 1
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

test_that("output", {
  gen <- odin::odin({
    initial(x[]) <- x0[i]
    update(x[]) <- x[i] + r[i]
    x0[] <- user()
    r[] <- user()
    dim(x0) <- user()
    dim(x) <- length(x0)
    dim(r) <- length(x)
    output(total) <- sum(x)
  }, verbose = TEST_VERBOSE)

  x0 <- runif(10)
  r <- runif(length(x0))

  mod <- gen(x0 = x0, r = r)

  tt <- 0:10
  yy <- mod$run(tt)
  zz <- mod$transform_variables(yy)

  expect_equal(zz$x, t(outer(r, tt) + x0))
  expect_equal(zz$total, rowSums(zz$x))
})
