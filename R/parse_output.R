## TODO: This needs a huge tidy up, but until we get some examples that:
##
## * use delay expressions together with output
## * use an array as a dependency of an output
## * use an array as an output
##
## These can get added soonish.  See issue #18
odin_parse_output <- function(obj) {
  if (!obj$info$has_output) {
    return(obj)
  }

  err <- obj$traits[, "is_output"] & obj$names_target %in% obj$vars
  if (any(err)) {
    odin_error("output() name cannot be the same as variable name",
               get_lines(obj$eqs[err]), get_exprs(obj$eqs[err]))
  }

  info <- odin_parse_extract_order(obj, TRUE)

  ## Modify the input here only:
  obj$output_info <- info

  ## Hmm, I'm not sure that we need or want to do this any more?
  tmp <- names_if(obj$traits[, "is_output"])
  obj$stage[tmp] <- STAGE_OUTPUT
  for (i in tmp) {
    obj$eqs[[i]]$stage <- STAGE_OUTPUT
  }

  obj
}
