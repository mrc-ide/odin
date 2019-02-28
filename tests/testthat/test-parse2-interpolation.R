context("parse: interpolation")

test_that("interpolation parse errors: incorrect array dimension", {
  expect_error(odin_parse2({
    deriv(y[]) <- pulse[i]
    initial(y[]) <- 0
    ##
    pulse[] <- interpolate(tp, zp, "constant")
    ##
    tp[] <- user()
    zp[] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    dim(pulse) <- 2
    dim(y) <- 2
  }),
  "Expected zp to be a 2 dimensional array")

  expect_error(odin_parse2({
    deriv(y[]) <- pulse[i]
    initial(y[]) <- 0
    ##
    pulse[] <- interpolate(tp, zp, "constant")
    ##
    tp[] <- user()
    zp[,,] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    dim(pulse) <- 2
    dim(y) <- 2
  }),
  "Expected zp to be a 2 dimensional array")

  expect_error(odin_parse2({
    deriv(y[]) <- pulse[i]
    initial(y[]) <- 0
    ##
    pulse[] <- interpolate(tp, zp, "constant")
    ##
    tp[,] <- user()
    zp[,] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    dim(pulse) <- 2
    dim(y) <- 2
  }),
  "Expected tp to be a vector")
})


test_that("unknown interpolation variable", {
  expect_error(odin_parse2({
    deriv(y[]) <- pulse[i]
    initial(y[]) <- 0
    ##
    pulse[] <- interpolate(tp, zp, "constant")
    ##
    tp[] <- user()
    dim(tp) <- user()
    dim(pulse) <- 2
    dim(y) <- 2
  }),
  "Unknown variable zp")
})


test_that("interpolation array assignment error", {
  expect_error(odin_parse2({
    deriv(y[]) <- pulse[i]
    initial(y[]) <- 0
    ##
    pulse[] <- interpolate(tp, zp, "constant")
    pulse[2] <- 3
    ##
    tp[] <- user()
    dim(tp) <- user()
    dim(pulse) <- 2
    dim(y) <- 2
  }),
  "interpolate() may only be used on a single-line array", fixed = TRUE)
})
