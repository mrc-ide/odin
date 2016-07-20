context("interpolation")

test_that("constant", {
  skip("not implemented")

  gen <- odin({
    deriv(y) <- pulse(t)
    initial(y) <- 0

    pulse <- interpolate(tp, zp, 0)

    tp[] <- user()
    zp[] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
  })
})
