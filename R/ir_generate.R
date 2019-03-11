## These are temporary!
odin2 <- function(x, validate = NULL, verbose = NULL, target = NULL,
                  pretty = NULL, compiler_warnings = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin2_(xx, validate, verbose, target)
}


odin2_ <- function(x, validate = NULL, verbose = NULL, target = NULL,
                   pretty = NULL, compiler_warnings = NULL) {
  opts <- odin_options(validate = validate,
                       verbose = verbose,
                       target = target,
                       pretty = pretty,
                       compiler_warnings = compiler_warnings)

  ir <- odin_parse2(x, opts$validate, opts$pretty)
  dat <- ir_deserialise(ir)

  message("target: ", opts$target)

  switch(opts$target,
         "r" = generate_r(dat),
         "c" = generate_c(dat, opts$verbose, opts$compiler_warnings),
         stop(sprintf("Unknown target '%s'", opts$target)))
}


odin_parse2 <- function(x, validate = NULL, pretty = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin_parse2_(xx, validate, pretty)
}


odin_parse2_ <- function(x, validate = NULL, pretty = NULL) {
  opts <- odin_options(validate = validate, pretty = pretty)
  odin_build_ir2(x, validate = opts$validate, pretty = pretty)
}


odin_options <- function(validate = NULL, verbose = NULL, target = NULL,
                         compiler_warnings = NULL, pretty = NULL) {
  defaults <- list(
    validate = FALSE,
    verbose = TRUE,
    target = "r",
    pretty = FALSE,
    compiler_warnings = FALSE)
  args <- list(validate = validate,
               verbose = verbose,
               target = target,
               pretty = pretty,
               compiler_warnings = compiler_warnings)
  stopifnot(setequal(names(defaults), names(args)))

  for (i in names(defaults)) {
    if (is.null(args[[i]])) {
      args[[i]] <- getOption(paste0("odin.", i), defaults[[i]])
    }
  }
  args
}
