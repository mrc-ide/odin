##' For lower-level odin functions \code{\link{odin_parse}},
##' \code{\link{odin_validate}} we accept a list of options rather
##' than individually named options.
##'
##' @title Odin options
##'
##' @inheritParams odin
##'
##' @param options Named list of options.  If provided, then all other
##'   options are ignored.
##'
##' @export
##' @examples
##' odin_options()
odin_options <- function(verbose = NULL, target = NULL, workdir = NULL,
                         validate = NULL, pretty = NULL, skip_cache = NULL,
                         compiler_warnings = NULL,
                         no_check_unused_equations = NULL,
                         no_check_naked_index = NULL,
                         options = NULL) {
  default_target <-
    if (is.null(target) && !can_compile(verbose = FALSE)) "r" else "c"
  defaults <- list(
    validate = FALSE,
    verbose = TRUE,
    target = default_target,
    workdir = tempdir(),
    pretty = FALSE,
    skip_cache = FALSE,
    no_check_unused_equations = FALSE,
    no_check_naked_index = FALSE,
    compiler_warnings = FALSE)
  if (is.null(options)) {
    options <- list(validate = validate,
                 verbose = verbose,
                 target = target,
                 pretty = pretty,
                 workdir = workdir,
                 skip_cache = skip_cache,
                 no_check_unused_equations = no_check_unused_equations,
                 no_check_naked_index = no_check_naked_index,
                 compiler_warnings = compiler_warnings)
  }
  stopifnot(setequal(names(defaults), names(options)))

  for (i in names(defaults)) {
    if (is.null(options[[i]])) {
      options[[i]] <- getOption(paste0("odin.", i), defaults[[i]])
    }
  }
  options
}
