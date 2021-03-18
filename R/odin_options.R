##' For lower-level odin functions [odin::odin_parse],
##' [odin::odin_validate] we only accept a list of options rather
##' than individually named options.
##'
##' @title Odin options
##'
##' @inheritParams odin
##'
##' @param rewrite_dims Logical, indicating if odin should try and
##'   rewrite your model dimensions (if using arrays). If `TRUE` then
##'   we replace dimensions known at compile-time with literal
##'   integers, and those known at initialisation with simplified and
##'   shared expressions. You may get less-comprehensible error
##'   messages with this option set to `TRUE` because parts of the
##'   model have been effectively evaluated during processing.
##'
##' @param rewrite_constants Logical, indicating if odin should try
##'   and rewrite *all* constant scalars. This is a superset of
##'   `rewrite_dims` and may be slow for large models. Doing this will
##'   make your model less debuggable; error messages will reference
##'   expressions that have been extensively rewritten, some variables
##'   will have been removed entirely or merged with other identical
##'   expressions, and the generated code may not be obviously
##'   connected to the original code.
##'
##' @param substitutions Optionally, a list of values to substitute
##'   into model specification as constants, even though they are
##'   declared as `user()`. This will be most useful in conjunction
##'   with `rewrite_dims` to create a copy of your model with
##'   dimensions known at compile time and all loops using literal
##'   integers.
##'
##' @return A list of parameters, of class `odin_options`
##'
##' @export
##' @examples
##' odin_options()
odin_options <- function(verbose = NULL, target = NULL, workdir = NULL,
                         validate = NULL, pretty = NULL, skip_cache = NULL,
                         compiler_warnings = NULL,
                         no_check_unused_equations = NULL,
                         rewrite_dims = NULL, rewrite_constants = NULL,
                         substitutions = NULL, options = NULL) {
  default_target <-
    if (is.null(target) && !can_compile(verbose = FALSE)) "r" else "c"
  defaults <- list(
    validate = FALSE,
    verbose = TRUE,
    target = default_target,
    workdir = tempfile(),
    pretty = FALSE,
    skip_cache = FALSE,
    rewrite_dims = FALSE,
    rewrite_constants = FALSE,
    substitutions = NULL,
    no_check_unused_equations = FALSE,
    compiler_warnings = FALSE)
  if (is.null(options)) {
    options <- list(
      validate = assert_scalar_logical_or_null(validate),
      verbose = assert_scalar_logical_or_null(verbose),
      target = target,
      pretty = assert_scalar_logical_or_null(pretty),
      workdir = workdir,
      skip_cache = assert_scalar_logical_or_null(skip_cache),
      rewrite_dims = assert_scalar_logical_or_null(rewrite_dims),
      rewrite_constants = assert_scalar_logical_or_null(rewrite_constants),
      substitutions = check_substitutions(substitutions),
      no_check_unused_equations =
        assert_scalar_logical_or_null(no_check_unused_equations),
      compiler_warnings = assert_scalar_logical_or_null(compiler_warnings))
  }
  stopifnot(all(names(defaults) %in% names(options)))

  for (i in names(defaults)) {
    if (is.null(options[[i]]) && i != "substitutions") {
      options[[i]] <- getOption(paste0("odin.", i), defaults[[i]])
    }
  }

  if (is.null(options$read_include)) {
    options$read_include <- switch(
      options$target,
      c = read_include_c,
      r = read_include_r,
      read_include_unsupported(options$target))
  }

  class(options) <- "odin_options"
  options
}


check_substitutions <- function(substitutions) {
  if (is.null(substitutions)) {
    return(NULL)
  }
  assert_named(substitutions, TRUE)
  assert_is(substitutions, "list")
  ok <- vlapply(substitutions, function(x)
    is.numeric(x) && length(x) == 1L)
  if (any(!ok)) {
    stop("Invalid entry in substitutions: ",
         paste(squote(names_if(!ok)), collapse = ", "))
  }
  substitutions
}
