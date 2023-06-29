test_that("Can parse with differentiable parameters", {
  ir <- odin_parse({
    initial(x) <- 1
    update(x) <- rnorm(0, 0.1)
    d <- data()
    compare(d) ~ normal(0, scale)
    scale <- user(differentiate = TRUE)
  })

  d <- ir_deserialise(ir)
  expect_true(d$features$has_derivative)
})


test_that("can't differentiate integer parameters", {
  expect_error(odin_parse({
    initial(x) <- 1
    update(x) <- rnorm(0, 0.1)
    d <- data()
    compare(d) ~ normal(x, scale)
    scale <- user(differentiate = TRUE, integer = TRUE)
  }),
  "Can't differentiate integer parameters\\s+scale <-")
})


test_that("can't differentiate without compare", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- rnorm(x, scale)
      scale <- user(differentiate = TRUE)
    }),
    "You need a compare expression to differentiate!\\s+scale <-")
})


test_that("can't differentiate continuous time models", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      deriv(x) <- 1
      d <- data()
      compare(d) ~ normal(x, scale)
      scale <- user(differentiate = TRUE)
    }),
    "Can't use differentiate with continuous time models\\s+scale <-")
})


test_that("can't differentiate models with arrays", {
  err <- expect_error(
    odin_parse({
      initial(x[]) <- 1
      update(x[]) <- rnorm(x, 1)
      dim(x) <- 5
      d <- data()
      compare(d) ~ normal(sum(x), scale)
      scale <- user(differentiate = TRUE)
    }),
    "Can't use differentiate with models that use arrays")
  expect_match(err$message, "dim(x) <-", fixed = TRUE)
  expect_match(err$message, "scale <-", fixed = TRUE)
})
