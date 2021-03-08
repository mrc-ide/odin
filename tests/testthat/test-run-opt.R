context("odin: opt")

test_that("optimise dimensions away entirely", {
  options <- odin_options(rewrite_dims = TRUE)
  gen <- odin({
    n <- 2
    m <- 2
    deriv(S[, ]) <- 0
    deriv(I) <- S[n, m]
    dim(S) <- c(n, m)
    initial(S[, ]) <- S0[i, j]
    initial(I) <- 0
    S0[, ] <- user()
    dim(S0) <- c(n, m)
  }, options = options)



})
