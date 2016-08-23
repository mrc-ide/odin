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

  ## A bit of information:
  output_info <- list(used=used_output, only=only_output, deriv=used_deriv)

  ## Here we really need to know the length of these things but we
  ## don't have that yet.  Then at the beginning of the derivative
  ## function we should assign everything out of the appropriate
  ## pointer so we're going to need to store some offsets.

  ## So a new function "output_order" will return NULL or a vector of
  ## indices.  We throw a bunch more offsets into the parameter vector
  ## too I think.
  nms_target <- vcapply(obj$eqs[nms_output], function(x) x$lhs$name_target)
  is_array <- obj$traits[nms_output, "is_array"]

  ord <- rep(0, length(is_array))
  stage_dim <- rep(STAGE_CONSTANT, length(is_array))
  names(stage_dim) <- nms_output

  ## Array output is not really working (there's an issue!) so this is
  ## untested and doesn't work in any case.  See issue #18
  if (any(is_array)) {
    stop("FIXME")
    ## This needs rewriting, but not sure if it's triggered anywhere
    ## really.
    tmp <- vcapply(nms_output[is_array], array_dim_name)
    ord[is_array] <- match(tmp, names(obj$eqs))
    stage_dim[is_array] <- viapply(obj$eqs[tmp], function(x) x$stage)
  }

  i <- order(ord)
  nms_output <- nms_output[i]
  nms_target <- nms_target[i]
  is_array <- is_array[i]
  stage_dim <- stage_dim[i]

  ## TODO: This is duplicated from above and could be generalised.
  offset <- setNames(as.list(seq_along(is_array) - 1L), nms_output)
  f <- function(i) {
    if (i == 1L) {
      0L
    } else if (!is_array[[i - 1L]]) {
      offset[[i - 1L]] + 1L
    } else if (identical(offset[[i - 1L]], 0L)) {
      as.name(array_dim_name(nms_output[[i - 1L]]))
    } else {
      call("+",
           as.name(paste0("offset_", nms_output[[i - 1L]])),
           as.name(array_dim_name(nms_output[[i - 1L]])))
    }
  }
  for (i in which(is_array)) {
    offset[[i]] <- f(i)
  }

  offset_is_var <- !vlapply(offset, is.numeric)
  offset_use <- offset
  offset_use[offset_is_var] <- sprintf("offset_%s", nms_output[offset_is_var])

  total <- f(length(is_array) + 1L)
  total_is_var <- !is.numeric(total)
  total_use <- if (total_is_var) "dim" else total
  total_stage <- max(stage_dim)

  ## Update here:
  obj$output_info <- output_info
  obj$stage[only_output] <- STAGE_OUTPUT
  for (i in intersect(only_output, names(obj$eqs))) {
    obj$eqs[[i]]$stage <- STAGE_OUTPUT
  }

  obj$output_order <-
    list(order=nms_target,
         is_array=is_array,
         offset=offset,
         offset_use=offset_use,
         offset_is_var=offset_is_var,
         total=total,
         total_is_var=total_is_var,
         total_use=total_use,
         total_stage=total_stage)
  obj
}
