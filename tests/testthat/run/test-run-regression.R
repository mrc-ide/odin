context("run: %TARGET%: regression")

test_that("bug #78", {
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
  })

  parameters <- list(S0 = cbind(c(1, 2), c(3, 4)))
  mod <- gen(user = parameters)
  expect_equal(mod$deriv(0, mod$initial(0)),
               c(4, rep(0, 4)))
})


## 75
test_that("bug #75", {
  gen <- odin({
    deriv(S) <- 1
    deriv(I) <- 2
    deriv(R) <- 3

    initial(S) <- N - I - R
    initial(I) <- I0
    initial(R) <- 5

    N <- 100
    I0 <- 1
  })

  dat <- gen()$contents()
  expect_equal(dat$initial_S, 94)
  expect_equal(dat$initial_I, 1)
  expect_equal(dat$initial_R, 5)
})
