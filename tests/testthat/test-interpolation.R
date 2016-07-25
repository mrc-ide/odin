context("interpolation")

test_that("constant", {
  interpolate_prepare()

  set.seed(1)
  x <- as.numeric(0:5)
  y <- runif(length(x))
  xout <- runif(20, 0, 5)

  for (order in c(0L, 1L)) {
    rtype <- if (order == 0L) "constant" else "linear"
    res_c <- .Call("test_interpolate0", x, y, xout, order)
    res_r <- approx(x, y, xout, rtype)$y
    expect_identical(res_c, res_r)

    res_c <- .Call("test_interpolate0",
                   x, rbind(y, deparse.level=0), xout, order)
    expect_equal(nrow(res_c), 1)
    expect_identical(drop(res_c), res_r)

    y2 <- rbind(y, y, deparse.level=0)
    res_c2 <- .Call("test_interpolate0", x, y2, xout, order)
    expect_equal(nrow(res_c2), 2)
    expect_identical(res_c2[1, ], res_r)
    expect_identical(res_c2[2, ], res_r)

    y3 <- rbind(y, y * 2, deparse.level=0)
    res_c3 <- .Call("test_interpolate0", x, y3, xout, order)
    expect_equal(nrow(res_c3), 2)
    expect_identical(res_c3[1, ], res_r)
    expect_identical(res_c3[2, ], res_r * 2)
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
  ## messages.
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
