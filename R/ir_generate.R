## These are temporary!
odin2 <- function(x, validate = NULL, verbose = NULL, target = NULL,
                  pretty = NULL, compiler_warnings = NULL,
                  no_check_naked_index = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin2_(xx, validate, verbose, target, pretty, compiler_warnings,
         no_check_naked_index)
}


odin2_ <- function(x, validate = NULL, verbose = NULL, target = NULL,
                   pretty = NULL, compiler_warnings = NULL,
                   no_check_naked_index = NULL) {
  opts <- odin_options(validate = validate,
                       verbose = verbose,
                       target = target,
                       pretty = pretty,
                       no_check_naked_index = no_check_naked_index,
                       compiler_warnings = compiler_warnings)

  ir <- odin_parse2(x, opts)
  dat <- ir_deserialise(ir)

  message("target: ", opts$target)

  switch(opts$target,
         "r" = generate_r(dat, opts),
         "c" = generate_c(dat, opts),
         stop(sprintf("Unknown target '%s'", opts$target)))
}


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


odin_options <- function(validate = NULL, verbose = NULL, target = NULL,
                         compiler_warnings = NULL, pretty = NULL,
                         no_check_naked_index = NULL,
                         opts = NULL) {
  defaults <- list(
    validate = FALSE,
    verbose = TRUE,
    target = "r",
    pretty = FALSE,
    no_check_naked_index = FALSE,
    compiler_warnings = FALSE)
  if (is.null(opts)) {
    opts <- list(validate = validate,
                 verbose = verbose,
                 target = target,
                 pretty = pretty,
                 no_check_naked_index = no_check_naked_index,
                 compiler_warnings = compiler_warnings)
  }
  stopifnot(setequal(names(defaults), names(opts)))

  for (i in names(defaults)) {
    if (is.null(opts[[i]])) {
      opts[[i]] <- getOption(paste0("odin.", i), defaults[[i]])
    }
  }
  opts
}
