context("parse: rewriting")

test_that("log", {
  expect_error(odin_parse({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- log()
  }), "Expected 1-2 arguments in log call", class = "odin_error")

  expect_error(odin_parse({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- log(1, 2, 3)
  }), "Expected 1-2 arguments in log call", class = "odin_error")
})




test_that("rewrite arrays", {
  options <- odin_options(rewrite_dims = TRUE, validate = TRUE)
  ir <- odin_parse({
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
  gen <- odin_generate(ir, options)
  mod <- gen(S0 = matrix(runif(4), 2, 2))
  mod$run(0:10)

  options <- odin_options(validate = TRUE)
  ir <- odin_parse({
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

  odin_generate(ir, options)

  ir_deserialise(ir)

  expect_false(grepl("dim_S_1", ir))
  expect_false(grepl("dim_S", ir))
})


test_that("rewrite arrays with shared dimensions", {
  options <- odin_options(rewrite_dims = TRUE, validate = FALSE)
  ir <- odin_parse({
    n <- user(integer = TRUE)
    m <- user(integer = TRUE)
    deriv(x[, ]) <- 0
    deriv(y[, ]) <- 0
    initial(x[, ]) <- 0
    initial(y[, ]) <- 0
    dim(x) <- c(n, m)
    dim(y) <- c(n, m)
  }, options = options)
  gen <- odin_generate(ir, options)
  mod <- gen(n = 4, m = 5)
  mod$contents()

  static_eval(quote(a * 2 * 3))
  static_eval(quote(2 * 3 * a))


})
