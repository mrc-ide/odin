context("aaa")

test_that("generate c code", {
  gen <- odin({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2
  }, target = "c", verbose = TRUE)
  mod <- gen()
  expect_true("cfuns" %in% ls(environment(mod$initialize)$private))
})
