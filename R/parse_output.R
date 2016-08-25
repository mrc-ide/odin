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

  info <- odin_parse_extract_order(names_if(obj$traits[, "is_output"]), obj)
  info$used <- odin_parse_output_usage(obj)

  ## Modify the input here only:
  obj$output_info <- info
  obj$stage[info$used$output_only] <- STAGE_OUTPUT
  for (i in intersect(info$used$output_only, names(obj$eqs))) {
    obj$eqs[[i]]$stage <- STAGE_OUTPUT
  }

  obj
}

odin_parse_output_usage <- function(obj) {
  ## OK, what I need to find out here is:
  ##
  ##   * what is the full set of dependencies, including variables,
  ##     that are used in computing the output variables.
  ##
  ##   * what is *only* used in computing output variables
  ##
  ## This may change later...
  nms_output <- names_if(obj$traits[, "is_output"])
  nms_deriv <- names_if(obj$traits[, "is_deriv"])

  used_output <-
    setdiff(unique(c(unlist(obj$deps_rec[nms_output], use.names=FALSE),
                     nms_output)),
            TIME)
  used_output <- c(setdiff(used_output, names(obj$eqs)),
                   intersect(names(obj$eqs), used_output))
  used_deriv <- unique(c(unlist(obj$deps_rec[nms_deriv], use.names=FALSE),
                         nms_deriv))

  if (obj$info$has_delay) {
    used_delay <-
      unique(unlist(lapply(obj$eqs[obj$traits[, "uses_delay"]],
                           function(x) x$rhs$depends_delay$variables)))

  } else {
    used_delay <- character(0)
  }

  ## TODO: I think this needs an intersect to get the order correct,
  ## but that might be done OK in used_output
  output_only <- intersect(setdiff(used_output, c(used_deriv, used_delay)),
                           names_if(obj$stage == STAGE_TIME))
  output_exprs <- setdiff(used_output, c(obj$vars, nms_output))

  ## Still not sure exactly how these are used; not all may be needed.
  list(output=used_output,
       output_only=output_only,
       deriv=used_deriv,
       exprs=output_exprs)
}
