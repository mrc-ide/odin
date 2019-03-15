#!/usr/bin/env Rscript
devtools::load_all()
path <- "tests/testthat/ir"
re_ext <- "\\.R$"
files <- dir(path, full.names = TRUE, pattern = re_ext)
options <- odin_options(pretty = FALSE, validate = TRUE)
for (f in files) {
  message(f)
  ir <- suppressMessages(ir_parse(f, options))
  writeLines(ir, sub(re_ext, ".json", f))
}
