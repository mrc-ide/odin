context("odin")

## Tests of the approach against some known models.
test_that("constant model", {
  gen <- odin::odin({
    deriv(y) <- 0.5
    initial(y) <- 1
  }, verbose=FALSE, dest=tempdir())
  mod <- gen$new()
  expect_identical(mod$init, 1.0)
  expect_identical(mod$deriv(0.0, mod$init), 0.5)

  tt <- seq(0, 10, length.out=11)
  yy <- mod$run(tt)
  expect_equal(yy[, 2L], seq(1.0, length.out=length(tt), by=0.5))
})
