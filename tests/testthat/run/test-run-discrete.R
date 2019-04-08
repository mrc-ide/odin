context("run: %TARGET%: discrete")

test_that("basic", {
  gen <- odin({
    initial(x) <- 1
    update(x) <- x + 1
  })

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
  gen <- odin({
    initial(x[]) <- x0[i]
    update(x[]) <- x[i] + r[i]
    x0[] <- user()
    r[] <- user()
    dim(x0) <- user()
    dim(x) <- length(x0)
    dim(r) <- length(x)
    output(total) <- sum(x)
  })

  x0 <- runif(10)
  r <- runif(length(x0))

  mod <- gen(x0 = x0, r = r)

  tt <- 0:10
  yy <- mod$run(tt)
  zz <- mod$transform_variables(yy)

  expect_equal(zz$x, t(outer(r, tt) + x0))
  expect_equal(zz$total, rowSums(zz$x))
})

test_that("interpolate", {
  gen <- odin({
    initial(x) <- 0
    update(x) <- x + pulse
    pulse <- interpolate(sp, zp, "constant")
    sp[] <- user()
    zp[] <- user()
    dim(sp) <- user()
    dim(zp) <- length(sp)
  })

  sp <- c(0, 10, 20)
  zp <- c(0, 1, 0)
  expect_error(gen(sp = sp, zp = zp[1:2]), "Expected length 3 value for zp")
  expect_error(gen(sp = sp, zp = rep(zp, 2)), "Expected length 3 value for zp")

  mod <- gen(sp = sp, zp = zp)

  tt <- 0:30
  expect_error(mod$run(tt - 1L),
               "Integration times do not span interpolation")

  yy <- mod$run(tt)
  zz <- cumsum(ifelse(tt <= 10 | tt > 20, 0, 1))
  expect_equal(yy[, 2], zz)
})

test_that("use step in model", {
  gen <- odin({
    initial(x) <- step
    update(x) <- step + 1
  })

  mod <- gen()
  res <- mod$run(5:10)
  expect_equal(res[, "x"], res[, "step"])
})

## This is to avoid a regression with array_dim_name
test_that("2d array equations", {
  gen <- odin({
    initial(x[,]) <- x0[i, j]
    update(x[,]) <- x[i, j] + r[i, j]
    x0[,] <- user()
    r[,] <- user()
    dim(x0) <- user()
    dim(x) <- c(dim(x0, 1), dim(x0, 2))
    dim(r) <- c(dim(x0, 1), dim(x0, 2))
  })

  r <- matrix(runif(10), 2, 5)
  x0 <- matrix(runif(10), 2, 5)

  mod <- gen(x0 = x0, r = r)
  yy <- mod$run(0:10)

  expect_equal(mod$contents()$x0, x0)
  expect_equal(matrix(mod$initial(0), 2, 5), x0)

  expect_equal(unname(diff(yy)[1, ]), c(1, c(r)))
  expect_equal(unname(diff(yy)[10, ]), c(1, c(r)))
})

## This turns up in one of Neil's cases:
test_that("complex initialisation: scalar", {
  gen <- odin({
    initial(x1) <- norm_rand()
    r <- x1 * 2
    initial(x2) <- r + 1
    update(x1) <- x1 + r
    update(x2) <- x2 + r
  })

  gen2 <- odin({
    x1_0 <- user()
    initial(x1) <- x1_0
    r <- x1 * 2
    initial(x2) <- r + 1
    update(x1) <- x1 + r
    update(x2) <- x2 + r
  })

  set.seed(1)
  mod <- gen()

  v <- mod$initial(0)
  vv <- mod$transform_variables(v)

  set.seed(1)
  x1 <- rnorm(1)
  expect_equal(vv$x1, x1)
  expect_equal(vv$x2, x1 * 2 + 1)

  mod2 <- gen2(x1)
  v2 <- mod2$initial(0)
  expect_equal(v2, v)

  set.seed(1)
  z <- mod$run(0:5)
  z2 <- mod2$run(0:5)
  expect_equal(z, z2)

  ## TODO: we never actually check here that the values are correct.
  ## It's a bit of an odd model because r grows with x1
})

test_that("complex initialisation: vector", {
  gen <- odin({
    initial(x1[]) <- norm_rand()
    r[] <- x1[i] * 2
    initial(x2[]) <- r[i] + 1

    update(x1[]) <- x1[i] + r[i]
    update(x2[]) <- x2[i] + r[i]

    dim(x1) <- 10
    dim(r) <- length(x1)
    dim(x2) <- length(x1)
  })

  set.seed(1)
  mod <- gen()

  v <- mod$initial(0)
  vv <- mod$transform_variables(v)

  set.seed(1)
  cmp <- rnorm(10)
  expect_equal(vv$x1, cmp)
  expect_equal(vv$x2, cmp * 2 + 1)
  expect_equal(mod$contents()$r, cmp * 2)
})
