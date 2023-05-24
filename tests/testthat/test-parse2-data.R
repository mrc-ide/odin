test_that("Can parse with a data element", {
  ir <- odin_parse({
    initial(x) <- 1
    update(x) <- rnorm(0, 0.1)
    d <- data()
  })
  d <- ir_deserialise(ir)
  expect_length(d$equations, 3)
  expect_mapequal(
    d$equations$d,
    list(name = "d", type = "data", source = list(3),
         depends = NULL, lhs = "d", data = list(type = "real_type")))
  expect_true(d$features$has_data)
})


test_that("Can parse with a compare expression", {
  ir <- odin_parse({
    initial(x) <- 1
    update(x) <- rnorm(0, 0.1)
    d <- data()
    compare(d) ~ normal(0, 1)
  })
  d <- ir_deserialise(ir)

  expect_length(d$equations, 4)
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
