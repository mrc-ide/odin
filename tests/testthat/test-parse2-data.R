test_that("Can parse with a data element", {
  ir <- odin_parse({
    initial(x) <- 1
    update(x) <- rnorm(0, 0.1)
    d <- data()
  })
  d <- ir_deserialise(ir)
  expect_length(d$equations, 2)
  expect_true(d$features$has_data)
  expect_mapequal(
    d$data$elements$d,
    list(name = "d", location = "data", storage_type = "double",
         rank = 0L, dimnames = NULL, stage = "time"))
})


test_that("Can parse with a data element", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- rnorm(0, 0.1)
      d <- data(1, 2)
    }),
    "Calls to data() must have no arguments",
    fixed = TRUE)
})


test_that("Can parse with a compare expression", {
  ir <- odin_parse({
    initial(x) <- 1
    update(x) <- rnorm(0, 0.1)
    d <- data()
    compare(d) ~ normal(0, 1)
  })
  d <- ir_deserialise(ir)

  expect_length(d$equations, 3)
  expect_mapequal(
    d$equations$compare_d,
    list(name = "compare_d",
         type = "compare",
         source = list(4),
         depends = list(functions = character(), variables = "d"),
         lhs = "d",
         compare = list(distribution = "normal",
                        args = list(0, 1))))
})


test_that("correct components", {
  ir <- odin_parse({
    initial(x) <- 1
    update(x) <- rnorm(0, 2 * sd)
    d <- data()
    sd <- user()
    compare(d) ~ normal(x, sd)
  })
  d <- ir_deserialise(ir)
  expect_equal(
    d$components$update,
    list(variables = character(), equations = character()))
  expect_equal(
    d$components$compare,
    list(variables = "x", equations = "compare_d"))
})


test_that("can't refer to data outside of compare", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- rnorm(0, 2 * d)
      d <- data()
    }),
    "Data ('d') may only be referred to in compare expressions",
    fixed = TRUE)
})


test_that("compare expressions must use ~ not <-", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- rnorm(0, 0.1)
      d <- data()
      compare(d) <- normal(0, 1)
    }),
    "All compare() expressions must use '~' and not '<-' or '='",
    fixed = TRUE)
})


test_that("compare expressions must be a call", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- rnorm(0, 0.1)
      d <- data()
      compare(d) ~ normal
    }),
    "Expected rhs of compare() expression to be a call",
    fixed = TRUE)
})


test_that("compare expressions must be a call", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- rnorm(0, 0.1)
      d <- data()
      compare(d) ~ exciting(0, 1)
    }),
    "Expected rhs to be a valid distribution",
    fixed = TRUE)
})


test_that("negative binomial still can't use args", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- rnorm(0, 0.1)
      d <- data()
      compare(d) ~ normal(mean = 0, sd = 1)
    }),
    "Named argument calls not supported in odin",
    fixed = TRUE)
})
