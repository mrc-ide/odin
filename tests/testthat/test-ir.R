context("ir")

test_that("deserialise", {
  code <- c("initial(x) <- 1", "deriv(x) <- 1")
  ir <- odin_parse_(code, type = "text")
  expect_error(odin_ir_deserialise(as.character(ir)),
               "Expected a json string")

  res <- odin_ir_deserialise(ir)
  expect_identical(res$ir, ir)
})


test_that("Stage information included in IR", {
  ir <- odin_parse_("examples/array_odin.R",
                    options = odin_options(rewrite_constants = FALSE))
  dat <- odin_ir_deserialise(ir)
  expect_equal(dat$data$elements$N_age$stage, "constant")
  expect_equal(dat$data$elements$I_tot$stage, "time")
})
