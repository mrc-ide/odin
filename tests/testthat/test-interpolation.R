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
    config(base) <- "constant"
  }, ".")

})
