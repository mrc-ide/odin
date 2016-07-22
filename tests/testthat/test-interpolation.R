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
  skip("not implemented")

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
    config(base) <- "constant"
  }, ".", verbose=FALSE)

  ## NOTE: when doing the checks for spanning, the only thing that
  ## matters for constant interpolation is that the *minimum* time
  ## matches.
  ##
  ## TODO: I want this to work with tp[1] = 0 but that requires some
  ## tweakery with the interpolation functions;
  tp <- c(0, 1, 2)
  zp <- c(0, 1, 0)
  mod <- gen(tp=tp, zp=zp)
  tt <- seq(0, 3, length.out=301)
  ## OK, so this doesn't work, but it also doesn't crash either.  All
  ## I see is NA values at every point which suggests that the
  ## interpolants have not been correctly initialised.
  ##
  ## The interpolation bits are done correctly; perhaps this is a
  ## staging/dependency issue?
  yy <- mod$run(tt)

})
