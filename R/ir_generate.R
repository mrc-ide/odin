odin_parse2 <- function(x, opts = NULL, type = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin_parse2_(xx, opts)
}


odin_parse2_ <- function(x, opts = NULL, type = NULL) {
  opts <- odin_options(opts = opts)
  odin_build_ir2(x, opts)
}
