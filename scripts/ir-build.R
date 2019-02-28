#!/usr/bin/env Rscript
devtools::load_all()
path <- "tests/testthat/ir"
re_ext <- "\\.R$"
files <- dir(path, full.names = TRUE, pattern = re_ext)
for (f in files) {
  message(f)
  ir <- suppressMessages(odin_build_ir2(f, pretty = FALSE, validate = TRUE))
  writeLines(ir, sub(re_ext, ".json", f))
}
