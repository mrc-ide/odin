context("interpolation")

test_that("constant endpoints", {
  interpolate_prepare()
  set.seed(1)
  x <- as.numeric(0:5)
  eps <- 1e-8

  expect_identical(approx(x, x, x, "constant")$y, x)
  expect_identical(approx(x, x, x + eps, "constant", rule=2)$y, x)
  expect_identical(.Call("test_interpolate", x, x, x, 0L), x)
  expect_identical(.Call("test_interpolate", x, x, x + eps, 0L), x)
})

test_that("interpolation", {
  interpolate_prepare()

  set.seed(1)
  x <- as.numeric(0:5)
  y <- runif(length(x))
  xout <- runif(20, 0, 5)
  ## This set of points here has the advantage that it:
  ##   a. is out of order so excercises the search function
  ##   b. includes all original time points
  xout <- sample(seq(0, 5, length.out=101))

  rapprox <- list(
    function(x, y, xout) approx(x, y, xout, "constant"),
    function(x, y, xout) approx(x, y, xout, "linear"),
    function(x, y, xout) spline(x, y, xout=xout, method="natural"))

  for (type in c(0L, 1L, 2L)) {
    ## We're all good except that the constant interpolation is not
    ## quite correct in the case of identical time matches.
    fr <- rapprox[[type + 1]]
    res_c <- .Call("test_interpolate", x, y, xout, type)
    res_r <- fr(x, y, xout)$y
    expect_equal(res_c, res_r, tolerance=1e-12)

    res_c <- .Call("test_interpolate",
                   x, cbind(y, deparse.level=0), xout, type)
    expect_equal(dim(res_c), c(length(xout), 1))
    expect_equal(drop(res_c), res_r, tolerance=1e-12)

    ## This is where we get messy.
    y2 <- cbind(y, y, deparse.level=0)
    res_c2 <- .Call("test_interpolate", x, y2, xout, type)
    expect_equal(dim(res_c2), c(length(xout), 2))
    expect_equal(res_c2[, 1], res_r, tolerance=1e-12)
    expect_equal(res_c2[, 2], res_r, tolerance=1e-12)

    y3 <- cbind(y, y * 2, deparse.level=0)
    res_c3 <- .Call("test_interpolate", x, y3, xout, type)
    expect_equal(dim(res_c2), c(length(xout), 2))
    expect_equal(res_c3[, 1], res_r, tolerance=1e-12)
    expect_equal(res_c3[, 2], res_r * 2, tolerance=1e-12)
  }
})

test_that("constant", {
  gen <- odin({
    deriv(y) <- pulse
    initial(y) <- 0
    ##
    pulse <- interpolate(tp, zp, 0)
    ##
    tp[] <- user()
    zp[] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    output(p) <- pulse
  }, verbose=FALSE)

  ## NOTE: when doing the checks for spanning, the only thing that
  ## matters for constant interpolation is that the *minimum* time
  ## matches.  The minimum match should be that t[1] is not *smaller*
  ## than the smallest interpolation time (equality being OK).
  ##
  ## TODO: I want this to work with tp[1] = 0 but that requires some
  ## tweakery with the interpolation functions;
  ##
  ## TODO: need to check that at least 2 times are provided for each
  ## variable used as an interpolation variable.  Could do that in the
  ## interpolation creation itself but that will give obscure error
  ## messages.  Probably best to do this in the user stage as there's
  ## already some checking there.
  tp <- c(0, 1, 2)
  zp <- c(0, 1, 0)
  expect_error(gen(tp=tp, zp=zp[1:2]), "Expected zp to have length 3")
  expect_error(gen(tp=tp, zp=rep(zp, 2)), "Expected zp to have length 3")

  mod <- gen(tp=tp, zp=zp)

  tt <- seq(0, 3, length.out=301)
  expect_error(mod$run(tt - 0.1),
               "Integration times do not span interpolation")

  yy <- mod$run(tt)
  zz <- ifelse(tt < 1, 0, ifelse(tt > 2, 1, tt - 1))
  expect_equal(yy[, 2], zz, tolerance=1e-5)
})

test_that("constant array", {
  gen <- odin({
    deriv(y[]) <- pulse[i]
    initial(y[]) <- 0
    ##
    pulse[] <- interpolate(tp, zp, 0)
    ##
    tp[] <- user()
    zp[,] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    dim(pulse) <- 2
    dim(y) <- 2
    config(base) <- "ic2"
  }, verbose=FALSE)

  tp <- c(0, 1, 2)
  zp <- cbind(c(0, 1, 0),
              c(0, 2, 0))
  ## Two dimensions to check here:
  expect_error(gen(tp=tp, zp=zp[1:2, ]), "zp to have size 3")
  expect_error(gen(tp=tp, zp=zp[c(1:3, 1:3), ]), "zp to have size 3")
  expect_error(gen(tp=tp, zp=zp[, 1, drop=FALSE]), "zp to have size 2")
  expect_error(gen(tp=tp, zp=zp[, c(1:2, 1)]), "zp to have size 2")

  mod <- gen(tp=tp, zp=zp)

  tt <- seq(0, 3, length.out=301)
  expect_error(mod$run(tt - 0.1),
               "Integration times do not span interpolation")

  yy <- mod$run(tt)
  matplot(tt, yy[, -1], type="l")
  zz1 <- ifelse(tt < 1, 0, ifelse(tt > 2, 1, tt - 1))
  zz2 <- ifelse(tt < 1, 0, ifelse(tt > 2, 2, 2 * (tt - 1)))
  expect_equal(yy[, 2], zz1, tolerance=1e-5)
  expect_equal(yy[, 3], zz2, tolerance=1e-5)
})
