## These are temporary!
odin2 <- function(x, validate = NULL, verbose = NULL, target = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin2_(xx, validate, verbose, target)
}


odin2_ <- function(x, validate = NULL, verbose = NULL, target = NULL) {
  opts <- odin_options(validate = validate, verbose = verbose, target = target)

  ir <- odin_parse2(x, opts$validate)
  dat <- ir_deserialise(ir)

  switch(opts$target,
         "r" = generate_r(dat),
         "c" = generate_c(dat, opts$verbose),
         stop(sprintf("Unknown target '%s'", opts$target)))
}


odin_parse2 <- function(x, validate = NULL) {
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
  opts <- odin_options(validate = validate)
  odin_build_ir2(x, validate = opts$validate)
}


odin_options <- function(validate = NULL, verbose = NULL, target = NULL) {
  list(validate = validate %||% getOption("odin.validate", FALSE),
       verbose = verbose %||% getOption("odin.verbose", TRUE),
       target = target %||% getOption("odin.target", "r"))
}
