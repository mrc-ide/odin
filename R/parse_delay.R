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

  ## For now, go through and pull these out, but I think we can get
  ## them other ways, later, perhaps.
  delay_arrays <-
    unique(unlist(lapply(obj$eqs[uses_delay], function(x) x$delay$arrays)))
  delay_arrays <- setNames(sprintf("delay_%s", delay_arrays), delay_arrays)
  obj$delay_support <- list(arrays=delay_arrays)

  obj
}

odin_parse_delay_1 <- function(idx, obj) {
  x <- obj$eqs[[idx]]

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

  ## Here, it's really important to pull these out with the scalars
  ## first, then the arrays.
  vars_is_array <- obj$variable_order$is_array
  extract <- intersect(names(vars_is_array), deps) # retains ordering
  is_array <- vars_is_array[extract]
  len <- length(extract)
  size <- vector("list", len)
  offset <- vector("list", len)
  for (j in seq_len(len)) {
    if (!is_array[[j]]) {
      size[[j]] <- 1L
      offset[[j]] <- j - 1L
    } else {
      size[[j]] <- array_dim_name(extract[[j]])
      if (j == 1L || !is_array[[j - 1L]]) {
        offset[[j]] <- j - 1L
      } else {
        offset[[j]] <- size[[j - 1L]]
      }
    }
  }
  names(size) <- names(offset) <- extract

  deps <- setdiff(deps, extract)
  dep_is_array <- vcapply(obj$eqs[deps], function(x) x$lhs$type) == "array"

  x$delay <- list(idx=idx,
                  time=x$rhs$value_time,
                  extract=extract,
                  deps=deps,
                  dep_is_array=dep_is_array,
                  arrays=intersect(deps, names_if(obj$traits[, "is_array"])),
                  is_array=is_array,
                  size=size,
                  offset=offset)
  x
}
