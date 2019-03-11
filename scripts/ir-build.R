#!/usr/bin/env Rscript
devtools::load_all()
path <- "tests/testthat/ir"
re_ext <- "\\.R$"
files <- dir(path, full.names = TRUE, pattern = re_ext)
opts <- odin_options(pretty = FALSE, validate = TRUE)
for (f in files) {
  message(f)
  ir <- suppressMessages(odin_build_ir2(f, opts))
  writeLines(ir, sub(re_ext, ".json", f))
}
