## These two are temporary!
odin2 <- function(x, validate = TRUE, verbose = TRUE, target = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin2_(xx, validate, verbose, target)
}


odin2_ <- function(x, validate = TRUE, verbose = TRUE, target = NULL) {
  ir <- odin_parse2(x, validate)
  dat <- ir_deserialise(ir)

  target <- target %||% getOption("odin.target", "r")
  switch(target,
         "r" = generate_r(dat),
         "c" = generate_c(dat),
         stop(sprintf("Unknown target '%s'", target)))
}


odin_parse2 <- function(x, validate = TRUE) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin_parse2_(xx, validate)
}


odin_parse2_ <- function(x, validate = FALSE) {
  odin_build_ir2(x, validate = validate)
}
