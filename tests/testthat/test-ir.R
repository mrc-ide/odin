context("ir")

## NOTE: this is too harsh a test but might come in useful
test_that("reference sets are unchanged", {
  skip_on_travis()
  skip_on_appveyor()
  skip_on_cran()
  skip("reference cases")
  path <- "ir"
  re_ext <- "\\.json$"
  files <- dir(path, full.names = TRUE, pattern = re_ext)

  options <- odin_options(pretty = FALSE, validate = TRUE)

  for (f_j in files) {
    f <- sub(re_ext, ".R", f_j)
    ir <- as.character(suppressMessages(ir_parse(f, options)))
    cmp <- trimws(read_string(f_j))
    expect_equal(ir, cmp)
  }
})


test_that("deserialise", {
  code <- c("initial(x) <- 1", "deriv(x) <- 1")
  ir <- odin_parse_(code, type = "text")
  expect_error(odin_ir_deserialise(as.character(ir)),
               "Expected a json string")

  res <- odin_ir_deserialise(ir)
  expect_identical(res$ir, ir)
})
