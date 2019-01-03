context("odin: ir based generator")

test_that("trivial model", {
  gen <- odin2({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2
  })

  mod <- gen()
  expect_is(mod, "odin_model")
  expect_equal(mod$initial(0), 1)
  expect_equal(mod$initial(10), 1)
  expect_equal(mod$deriv(0, 0), 2)
  expect_equal(mod$deriv(10, 10), 2)
  tt <- 0:10
  yy <- mod$run(tt)
  expect_equal(colnames(yy), c("t", "y"))
  expect_equal(yy[, 1], tt)
  expect_equal(yy[, 2], seq(1, length.out = length(tt), by = 2))

  expect_equal(mod$contents(), list(initial_y = 1, r = 2))
})


## This tests a few things
##
## 1. can we use time dependent rhs
## 2. can we make transient variables work correctly
## 3. we can construct somewhat nontrivial expressions
##
## This should integrate to a parabola y = 1 + t^2
test_that("Time dependent rhs", {
  gen <- odin2({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2 * t
  })

  ## This looks like a reasonable rhs but it's going through the
  ## internal storage instead of being transient.
  mod <- gen()

  tt <- 0:10
  yy <- mod$run(tt, atol = 1e-8, rtol = 1e-8)
  expect_equal(yy[, 1], tt)
  expect_equal(yy[, 2], 1 + tt^2)

  ## NOTE: this is very implementation dependent but hopefully
  ## confirms that transient variables are dealt with appropriately
  expect_equal(body(r6_private(mod)$core$rhs_dde)[[3]],
               quote(r <- 2 * t))
  expect_equal(mod$contents(), list(initial_y = 1))
})


test_that("Time dependent initial conditions", {
  gen <- odin2({
    y1 <- cos(t)
    y2 <- y1 * (r + t)
    r <- 1
    deriv(y3) <- y2
    initial(y3) <- y2
  })

  mod <- gen()

  f <- function(t) {
    cos(t) * (1 + t)
  }

  expect_equal(mod$initial(0), f(0))
  expect_equal(mod$initial(1), f(1))
  expect_equal(mod$deriv(0, 1), f(0))
  expect_equal(mod$deriv(1, 1), f(1))

  expect_equal(mod$contents(),
               list(initial_y3 = f(1), r = 1))
})
