odin_options <- function(verbose = NULL, target = NULL, workdir = NULL,
                         validate = NULL, pretty = NULL, skip_cache = NULL,
                         compiler_warnings = NULL, no_check_naked_index = NULL,
                         opts = NULL) {
  default_target <-
    if (is.null(target) && !can_compile(verbose = FALSE)) "r" else "c"
  defaults <- list(
    validate = FALSE,
    verbose = TRUE,
    target = default_target,
    workdir = tempdir(),
    pretty = FALSE,
    skip_cache = FALSE,
    no_check_naked_index = FALSE,
    compiler_warnings = FALSE)
  if (is.null(opts)) {
    opts <- list(validate = validate,
                 verbose = verbose,
                 target = target,
                 pretty = pretty,
                 workdir = workdir,
                 skip_cache = skip_cache,
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
