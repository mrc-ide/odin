## These two are temporary!
odin2 <- function(x, validate = TRUE, verbose = TRUE) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin2_(xx, validate, verbose)
}


odin2_ <- function(x, validate = TRUE, verbose = TRUE) {
  ir <- odin_build_ir(x, validate = validate)
  dat <- ir_deserialise(ir)
  dat$ir <- ir
  generate_r(dat, validate)
}
