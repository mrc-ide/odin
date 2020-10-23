context("parse: interpolation")

test_that("interpolation parse errors: incorrect array dimension", {
  expect_error(odin_parse({
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
  "Expected zp to be a 2 dimensional array", class = "odin_error")

  expect_error(odin_parse({
    deriv(y[]) <- pulse[i]
    initial(y[]) <- 0
    ##
    pulse[] <- interpolate(tp, zp, "constant")
    ##
    tp[] <- user()
    zp[, , ] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    dim(pulse) <- 2
    dim(y) <- 2
  }),
  "Expected zp to be a 2 dimensional array", class = "odin_error")

  expect_error(odin_parse({
    deriv(y[]) <- pulse[i]
    initial(y[]) <- 0
    ##
    pulse[] <- interpolate(tp, zp, "constant")
    ##
    tp[, ] <- user()
    zp[, ] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    dim(pulse) <- 2
    dim(y) <- 2
  }),
  "Expected tp to be a vector", class = "odin_error")
})


test_that("unknown interpolation variable", {
  expect_error(odin_parse({
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
  "Unknown variable zp", class = "odin_error")
})


test_that("interpolation array assignment error", {
  expect_error(odin_parse({
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
  "interpolate() may only be used on a single-line array",
  fixed = TRUE, class = "odin_error")
})
