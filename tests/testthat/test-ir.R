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

  for (f_j in files) {
    f <- sub(re_ext, ".R", f_j)
    ir <- as.character(suppressMessages(
      odin_build_ir2(f, pretty = FALSE, validate = TRUE)))
    cmp <- trimws(readChar(f_j, file.size(f_j)))
    expect_equal(ir, cmp)
  }
})
