## TODO: This needs a huge tidy up, but until we get some examples that:
##
## * use delay expressions together with output
## * use an array as a dependency of an output
## * use an array as an output
##
## These can get added soonish.  See issue #18
##
## For now, this function remains a mess.
odin_parse_output <- function(obj) {
  if (!obj$info$has_output) {
    return(obj)
  }

  ## Then, we compute two subgraphs (for dde) in the case where there
  ## are output variables.  We'd actually only want to get the output
  ## variables that are time dependent I think, but that really should
  ## be all of them.

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

  only_output <- intersect(setdiff(used_output, c(used_deriv, used_delay)),
                           names_if(obj$stage == STAGE_TIME))

  ## A bit of information (TODO: may merge these?)
  obj$output_info <- list(used=used_output, only=only_output, deriv=used_deriv)
  obj$output_order <- odin_parse_extract_order(nms_output, obj)

  obj$stage[only_output] <- STAGE_OUTPUT
  for (i in intersect(only_output, names(obj$eqs))) {
    obj$eqs[[i]]$stage <- STAGE_OUTPUT
  }

  obj
}
