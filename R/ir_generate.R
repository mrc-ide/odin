## These two are temporary!
odin2 <- function(x, validate = TRUE, verbose = TRUE,
                  target = NULL, new_parser = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin2_(xx, validate, verbose, target, new_parser)
}


odin2_ <- function(x, validate = TRUE, verbose = TRUE,
                   target = NULL, new_parser = NULL) {
  ir <- odin_parse2(x, validate, new_parser)
  dat <- ir_deserialise(ir)

  target <- target %||% getOption("odin.target", "r")
  switch(target,
         "r" = generate_r(dat),
         "c" = generate_c(dat),
         stop(sprintf("Unknown target '%s'", target)))
}


odin_parse2 <- function(x, validate = FALSE, new_parser = NULL) {
  new_parser <- new_parser %||% getOption("odin.new_parser", TRUE)

  if (new_parser) {
    ir <- odin_build_ir2(x, validate = validate)
  } else {
    ir <- odin_build_ir(x, validate = validate)
  }
  ir
}
