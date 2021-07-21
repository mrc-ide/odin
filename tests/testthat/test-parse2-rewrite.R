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


test_that("rewrite arrays drops references to dim_ variables", {
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
  }, options = odin_options(rewrite_dims = TRUE,
                            rewrite_constants = FALSE))
  expect_false(grepl("dim_S_1", ir))
  expect_false(grepl("dim_S", ir))
})


test_that("Can create compile-time constants", {
  ir <- odin_parse({
    n <- user(integer = TRUE)
    m <- user()
    deriv(S[, ]) <- 0
    deriv(I) <- S[n, m]
    dim(S) <- c(n, m)
    initial(S[, ]) <- S0[i, j]
    initial(I) <- 0
    S0[, ] <- user()
    dim(S0) <- c(n, m)
  }, options = odin_options(rewrite_dims = TRUE,
                            rewrite_constants = FALSE,
                            substitutions = list(n = 2, m = 3)))
  dat <- ir_deserialise(ir)
  expect_equal(dat$equations$n$type, "expression_scalar")
})


test_that("Can validate substitutions", {
  code <- quote({
    n <- user(integer = TRUE, min = 2)
    m <- user(max = 10)
    a <- 1
    deriv(S[, ]) <- 0
    deriv(I) <- S[n, m]
    dim(S) <- c(n, m)
    initial(S[, ]) <- S0[i, j] * a
    initial(I) <- 0
    S0[, ] <- user()
    dim(S0) <- c(n, m)
  })

  expect_error(
    odin_parse(code, options = odin_options(substitutions = list(y = 1))),
    "Substitution failed: 'y' is not an equation")
  expect_error(
    odin_parse(code, options = odin_options(substitutions = list(a = 1))),
    "Substitution failed: 'a' is not a user() equation", fixed = TRUE)
  expect_error(
    odin_parse(code, options = odin_options(substitutions = list(S0 = 1))),
    "Substitution failed: 'S0' is an array", fixed = TRUE)
  expect_error(
    odin_parse(code, options = odin_options(substitutions = list(n = 1))),
    "Expected 'n' to be at least 2")
  expect_error(
    odin_parse(code, options = odin_options(substitutions = list(n = 2.4))),
    "Expected 'n' to be integer-like")
  expect_error(
    odin_parse(code, options = odin_options(substitutions = list(m = 20))),
    "Expected 'm' to be at most 10")
  expect_error(
    odin_parse(code,
               options = odin_options(substitutions = list(m = NA_real_))),
    "'m' must not contain any NA values")
  expect_error(
    odin_parse(code,
               options = odin_options(substitutions =
                                        list(m = NULL, n = NULL))),
    "Invalid entry in substitutions: 'm', 'n'")
  expect_error(
    odin_parse(code, options = odin_options(substitutions = list(1, 2))),
    "'substitutions' must be named")
  expect_error(
    odin_parse(code, options =
                       odin_options(substitutions = list(n = 1, n = 1))),
    "'substitutions' must have unique names")
  expect_error(
    odin_parse(code, options = odin_options(substitutions = c(n = 1))),
    "'substitutions' must be a list")
})


test_that("Rewrite all constants", {
  ir <- odin_parse({
    a <- 10
    b <- 20
    c <- 30
    initial(x) <- 0
    deriv(x) <- a + b * c
  }, options = odin_options(rewrite_constants = TRUE))
  dat <- ir_deserialise(ir)
  expect_length(dat$equations, 2)
  expect_setequal(names(dat$equations), c("initial_x", "deriv_x"))
  expect_equal(dat$equations$deriv_x$rhs$value, 610) # i.e., 10 + 20 * 30
})


test_that("leave time-varying expressions alone", {
  ir <- odin_parse({
    a <- 2 * t
    deriv(x) <- a * 3
    deriv(y) <- a * 4
    initial(x) <- 0
    initial(y) <- 0
  }, options = odin_options(rewrite_constants = TRUE))
  dat <- ir_deserialise(ir)
  expect_equal(
    dat$equations$deriv_x$rhs$value,
    list("*", "a", 3))
  expect_equal(
    dat$equations$deriv_y$rhs$value,
    list("*", "a", 4))
})

test_that("collapse complex constants into expressions", {
  ir <- odin_parse({
    a <- 2 * t
    b <- 2 * n
    n <- 4
    deriv(x) <- a + b
    initial(x) <- 0
  }, options = odin_options(rewrite_constants = TRUE))
  dat <- ir_deserialise(ir)
  expect_equal(
    dat$equations$deriv_x$rhs$value,
    list("+", "a", 8))
})


test_that("collapse if/else expressions", {
  code <- c(
    "a <- user()",
    "b <- if (a == 1) 2 else 3",
    "initial(x) <- 1",
    "deriv(x) <- x * b")

  ir1 <- odin_parse_(code,
                     options = odin_options(rewrite_constants = TRUE,
                                            substitutions = list(a = 1)))
  ir2 <- odin_parse_(code,
                     options = odin_options(rewrite_constants = TRUE,
                                            substitutions = list(a = 2)))

  expect_equal(ir_deserialise(ir1)$equations$deriv_x$rhs$value,
               list("*", "x", 2))
  expect_equal(ir_deserialise(ir2)$equations$deriv_x$rhs$value,
               list("*", "x", 3))
})
