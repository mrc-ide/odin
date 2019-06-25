context("ir")

test_that("deserialise", {
  code <- c("initial(x) <- 1", "deriv(x) <- 1")
  ir <- odin_parse_(code, type = "text")
  expect_error(odin_ir_deserialise(as.character(ir)),
               "Expected a json string")

  res <- odin_ir_deserialise(ir)
  expect_identical(res$ir, ir)
})
