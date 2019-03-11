context("parse: inplace")

test_that("can't use integer inplace for update()", {
  expect_error(odin_parse2({
    q[] <- user()
    p[] <- q[i] / sum(q)
    initial(x[]) <- 0
    update(x[]) <- rmultinom(5, p)
    dim(p) <- 5
    dim(q) <- 5
    dim(x) <- 5
  }),
  "Can't use inplace integer expression in update")
})


test_that("rmultinom is integer", {
  ir <- odin_parse2({
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
