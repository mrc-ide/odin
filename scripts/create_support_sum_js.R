#!/usr/bin/env Rscript
devtools::load_all()
code <- unlist(lapply(2:8, generate_js_support_sum))
writeLines(code, "inst/js/support_sum.js")
