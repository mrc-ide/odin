## Delay variables can't depend on delay variables (I believe this is
## checked somewhere).  That simplifies things because we can't have:
##
##   x <- delay(a, t)
##   y <- delay(x, t)
##
## and then have to extract the delayed variable over the delayed
## variable!  That would just be too much for deSolve to deal with I
## think.
##
## The ordering of delay variables is based on the lag time.  Delay
## variables will be computed before they are needed for the rest of
## the variables but ideally we'd collect all variables that
## correspond to a particular time to allow sharing of computation.
## So if there are two delay variables with the same lag time then
## we'd extract the pair of them together:
##
##   x <- delay(a + c, t)
##   y <- delay(b + c, t)
##
## will group x and y together so that the lookup happens at once:
##
##   {
##      ... pull a, b, c out of the delay loop ...
##      ... compute x and y ...
##   }
##
## however, that complicates things significantly for gains that might
## not be that apparent.  In particular we would need to reorder the
## expressions computed on underlying a, b, c above and get the
## topological order there correct.  I think that can be looked up
## against the order vector though.  I'm really in two minds about
## this because I don't see it being used much in the BM code that I
## have seen, and because correct is better than fast.  For now, we
## don't combine.
odin_parse_delay <- function(obj) {
  if (!obj$info$has_delay) {
    return(obj)
  }
  ## Lots of ugly processing here.  The idea here is to order the
  ## time-dependent variables correctly within each delay block and to
  ## filter out any non-time-dependent things.  This is annoyingly
  ## dependent on many function variables and so hard to functionalise
  ## as a result.
  ##
  ## I think we'd do better if this entirely moved into the delay
  ## processing code, really.
  ##
  ## To do that we'll need *at least* deps_rec and stage moved into
  ## the object (which is not a bad thing anyway).

  uses_delay <- obj$traits[, "uses_delay"]
  obj$eqs[uses_delay] <- lapply(which(uses_delay), odin_parse_delay_1, obj)

  ## Determine all delay equation dependencies that are delays, and
  ## indicate this in their dim() element.  The reason it goes there
  ## is because we'll need to arrange additional array storage for all
  ## of these.
  delay_array_deps <-
    unique(unlist(lapply(obj$eqs[uses_delay],
                         function(x) names_if(x$delay$deps_is_array))))
  for (nm in delay_array_deps) {
    obj$eqs[[array_dim_name(nm)]]$used_in_delay <- TRUE
  }

  obj
}

odin_parse_delay_1 <- function(idx, obj) {
  x <- obj$eqs[[idx]]

  msg <- setdiff(x$rhs$depends_delay$variables,
                 c(names(obj$deps_rec), obj$vars, INDEX))
  if (length(msg) > 0L) {
    odin_error(sprintf("Missing %s in delay expression: %s",
                       ngettext(length(msg), "variable", "variables"),
                       pastec(msg)),
               x$line, x$expr)
  }

  ## Compute all the dependencies of this equation (and all their
  ## dependencies) filtered by those that depend on time.
  deps <- intersect(x$rhs$depends_delay$variables, names(obj$deps_rec))
  deps <- unique(c(deps, unlist(obj$deps_rec[deps], use.names=FALSE)))
  ## NOTE: We have to exclude delayed values from the dependencies
  ## here, even though they are time dependent (*different* sort
  ## of time dependence...)
  ##
  ## TODO: I don't know if delay depending on delay is tested for
  ## anywhere?
  deps <- setdiff(deps[obj$stage[deps] == STAGE_TIME],
                  c(TIME, names_if(obj$traits[, "uses_delay"])))
  deps <- deps[order(match(deps, names(obj$deps_rec)))]

  ## TODO: this overlaps with the new function "odin_parse_extract_order".
  ## Here, it's really important to pull these out with the scalars
  ## first, then the arrays.
  var_extract <- intersect(obj$variable_info$order, deps) # retains ordering
  var_is_array <-
    obj$variable_info$is_array[match(var_extract, obj$variable_info$order)]

  len <- length(var_extract)
  var_size <- vector("list", len)
  var_offset <- vector("list", len)
  for (j in seq_len(len)) {
    if (!var_is_array[[j]]) {
      var_size[[j]] <- 1L
      var_offset[[j]] <- j - 1L
    } else {
      var_size[[j]] <- array_dim_name(var_extract[[j]])
      if (j == 1L || !var_is_array[[j - 1L]]) {
        var_offset[[j]] <- j - 1L
      } else {
        var_offset[[j]] <- var_size[[j - 1L]]
      }
    }
  }
  names(var_size) <- names(var_offset) <- var_extract

  ## Non-variable dependencies:
  deps <- setdiff(deps, var_extract)
  ## NOTE: setNames is needed here because they may otherwise be
  ## dropped.  This might cause bugs elsewhere so watch for usage of
  ## indexing traits when names are required...
  deps_is_array <- setNames(obj$traits[deps, "is_array"], deps)

  x$delay <- list(idx=idx,
                  time=x$rhs$value_time,
                  ## variables:
                  ## TODO: in generate, shift to the var_ prefix (or nest)
                  extract=var_extract,
                  is_array=var_is_array,
                  size=var_size,
                  offset=var_offset,
                  ## other dependencies:
                  deps=deps,
                  deps_is_array=deps_is_array)
  x
}
