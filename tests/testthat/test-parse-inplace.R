context("parse: inplace")

test_that("can't use integer inplace for update()", {
  expect_error(odin_parse({
    q[] <- user()
    p[] <- q[i] / sum(q)
    initial(x[]) <- 0
    update(x[]) <- rmultinom(5, p)
    dim(p) <- 5
    dim(q) <- 5
    dim(x) <- 5
  }),
  "Can't use inplace integer expression in update",
  class = "odin_error")
})


test_that("can't use multiline inplace", {
  expect_error(odin_parse({
    q[] <- user()
    p[] <- q[i] / sum(q)
    initial(x[]) <- 0
    update(x[]) <- y[i]
    y[] <- rmultinom(5, p)
    y[1] <- 0
    dim(p) <- 5
    dim(q) <- 5
    dim(x) <- 5
    dim(y) <- 5
  }),
  "in-place equations may only be used on a single-line array",
  class = "odin_error")
})


test_that("rmultinom is integer", {
  ir <- odin_parse({
    q[] <- user()
    p[] <- q[i] / sum(q)
    initial(x[]) <- 0
    update(x[]) <- y[i]
    y[] <- rmultinom(5, p)
    dim(p) <- 5
    dim(q) <- 5
    dim(x) <- 5
    dim(y) <- 5
  })
  dat <- ir_deserialise(ir)
  expect_equal(dat$data$elements$y$storage_type, "int")
  expect_equal(dat$data$elements$y$rank, 1)
})


test_that("validate input arrays to inplace expressions", {
  expect_error(
    odin_parse({
      update(x) <- 1
      initial(x) <- 1
      y[] <- rmultinom(2, x)
      dim(y) <- 2
    }),
    "Expected an array of rank 1 for argument 2 of 'rmultinom' (got rank 0)",
    fixed = TRUE, class = "odin_error")

  expect_error(
    odin_parse({
      update(x) <- 1
      initial(x) <- 1
      y[] <- rmultinom(p, p)
      dim(y) <- 2
      p[] <- user()
      dim(p) <- 2
    }),
    "Expected a scalar for argument 1 of 'rmultinom' (got array of rank 1)",
    fixed = TRUE, class = "odin_error")
})



test_that("in place expressions must be simple", {
  expect_error(
    odin_parse({
      update(x) <- 1
      initial(x) <- 1
      y <- rmultinom(5 + 2, x)
    }),
    "At present, inplace function 'rmultinom' must use no functions",
    class = "odin_error")
})


test_that("in place expressions must return an array", {
  expect_error(
    odin_parse({
      update(x) <- 1
      initial(x) <- 1
      p[] <- user()
      dim(p) <- 2
      y <- rmultinom(2, p)
    }),
    "Expected an array of rank 1 the lhs of inplace function 'rmultinom'",
    class = "odin_error")
})
