test_that("delays", {
  gen <- odin2({
    initial(y) <- 1
    update(y) <- y + yprev
    yprev <- delay(y, 1)
  }, verbose = TEST_VERBOSE)

  mod <- gen()

  tt <- seq(0:10)
  yy <- mod$run(tt)
  expect_equal(yy[, "y"], c(1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144))
})

## This also catches a corner case in the inclusion of sum() in the
## headers.
test_that("delays: scalar variable", {
  gen <- odin2({
    r <- 3.6
    update(y) <- r * y * (1 - y)
    initial(y) <- 0.2
    x <- delay(y, 2)
    output(x) <- TRUE
  }, verbose = TEST_VERBOSE)

  mod <- gen()
  tt <- seq(0:20)
  yy <- mod$transform_variables(mod$run(tt))

  ## Check that the underlying data are correct:
  dat <- mod$contents()
  cmp <- logistic_map(dat$r, dat$initial_y, diff(range(tt)))

  ## Then check the delayed expression:
  i <- seq_len(length(tt) - 2)
  expect_equal(yy$x[i + 2], yy$y[i])
})

test_that("delays: scalar expression", {
  gen <- odin2({
    r <- 3.6
    update(y[]) <- r * y[i] * (1 - y[i])
    initial(y[1]) <- 0.2
    initial(y[2]) <- 0.4
    x <- delay(sum(y) / length(y), 2)
    output(x) <- TRUE
    dim(y) <- 2
  }, verbose = TEST_VERBOSE)

  mod <- gen()
  tt <- seq(0:20)
  yy <- mod$transform_variables(mod$run(tt))

  ## Check that the underlying data are correct:
  dat <- mod$contents()
  cmp <- logistic_map(dat$r, dat$initial_y, diff(range(tt)))

  ## Then check the delayed expression:
  i <- seq_len(length(tt) - 2)
  expect_equal(yy$x[i + 2], rowMeans(yy$y[i, ]))
})

test_that("delays: vector variable", {
  skip("discrete delays")
  gen <- odin2({
    r <- 3.6
    update(y[]) <- r * y[i] * (1 - y[i])
    initial(y[1]) <- 0.2
    initial(y[2]) <- 0.4
    x[] <- delay(y[i], 2)
    output(x[]) <- TRUE
    dim(y) <- 2
    dim(x) <- 2
  }, verbose = TEST_VERBOSE)

  mod <- gen()
  tt <- seq(0:20)
  yy <- mod$transform_variables(mod$run(tt))

  ## Check that the underlying data are correct:
  dat <- mod$contents()
  cmp <- logistic_map(dat$r, dat$initial_y, diff(range(tt)))

  ## Then check the delayed expression:
  i <- seq_len(length(tt) - 2)
  expect_equal(yy$x[i + 2, ], yy$y[i, ])
})

test_that("delays: vector expression", {
  skip("discrete delays")
  gen <- odin2({
    r <- 3.6
    update(y[]) <- r * y[i] * (1 - y[i])
    initial(y[1]) <- 0.2
    initial(y[2]) <- 0.4
    x[] <- delay(y[i] / sum(y), 2)
    output(x[]) <- TRUE
    dim(x) <- 2
    dim(y) <- 2
  }, verbose = TEST_VERBOSE)

  mod <- gen()
  tt <- seq(0:20)
  yy <- mod$transform_variables(mod$run(tt))

  ## Check that the underlying data are correct:
  dat <- mod$contents()
  cmp <- logistic_map(dat$r, dat$initial_y, diff(range(tt)))
  expect_equal(yy$y, cmp)

  ## Then check the delayed expression:
  i <- seq_len(length(tt) - 2)
  expect_equal(yy$x[i + 2, ], yy$y[i, ] / rowSums(yy$y[i, ]))
})

test_that("delay vars that depend on time", {
  gen <- odin2({
    initial(x) <- 0
    update(x) <- x + v

    v <- if (step < 5) 0 else 1
    y <- delay(v, 2)
    output(y) <- TRUE
  }, verbose = TEST_VERBOSE)

  mod <- gen()
  tt <- 0:10
  yy <- mod$run(tt)

  expect_equal(yy[, "x"], ifelse(tt < 6, 0, tt - 5))
  expect_equal(yy[, "y"], ifelse(tt < 7, 0, 1))
})
