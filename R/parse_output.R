odin_parse_output <- function(obj) {
  if (!obj$info$has_output) {
    return(obj)
  }

  err <- obj$traits[, "is_output"] & obj$names_target %in% obj$vars
  if (any(err)) {
    odin_error("output() name cannot be the same as variable name",
               get_lines(obj$eqs[err]), get_exprs(obj$eqs[err]))
  }

  ## Modify the input here only:
  obj$output_info <- odin_parse_extract_order(obj, TRUE)

  obj
}
