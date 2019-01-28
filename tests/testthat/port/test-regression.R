context("regression tests")

test_that("bug #78", {
  gen <- odin2({
    n <- 2
    m <- 2
    deriv(S[, ]) <- 0
    deriv(I) <- S[n,m]
    dim(S) <- c(n,m)
    initial(S[, ]) <- S0[i, j]
    initial(I) <- 0
    S0[, ] <- user()
    dim(S0) <- c(n, m)
  }, verbose = TEST_VERBOSE)

  parameters <- list(S0 = cbind(c(1, 2), c(3, 4)))
  mod <- gen(user = parameters)
  expect_equal(mod$deriv(0, mod$initial(0)),
               c(4, rep(0, 4)))
})
