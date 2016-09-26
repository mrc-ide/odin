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
                         function(x) names_if(x$delay$expr$deps_is_array))))
  for (nm in delay_array_deps) {
    obj$eqs[[array_dim_name(nm)]]$used_in_delay <- TRUE
  }

  obj
}

odin_parse_delay_1 <- function(idx, obj) {
  x <- obj$eqs[[idx]]
  ## TODO: why is this separate from the ordinary checks?  Do we need
  ## to do the same thing for the time check?  What about the default
  ## once that turns up?  It's all so confusing.
  tmp <- join_deps(list(x$rhs$depends_delay, x$rhs$depends_default,
                        x$rhs$depends_time))
  msg <- setdiff(tmp$variables, c(names(obj$deps_rec), obj$vars, INDEX))
  if (length(msg) > 0L) {
    odin_error(sprintf("Missing %s in delay expression: %s",
                       ngettext(length(msg), "variable", "variables"),
                       pastec(msg)),
               x$line, x$expr)
  }

  ## TODO: probably time needs processing, but it should be processed
  ## in the same way as the main expression because it's processed at
  ## the top level (ish) of the function.
  expr <- odin_parse_delay_1_depends(x$rhs$depends_delay$variables, obj)

  ## This is very similar, but also quite different, to
  ## variable_offsets, but because we know we need everything always
  ## we don't need to store and track new offsets.  Instead we'll
  ## unpack and move the pointer along from the last array.  This does
  ## exploit that scalars are packed in first.
  delay_offset <- function(i) {
    if (identical(expr$offset[[i]], 0L) && expr$is_array[[i]]) {
      NULL # special treatment to avoid (foo + 0)
    } else if (is.numeric(expr$offset[[i]])) {
      ## This is going to exploit the fact that all non-array cases
      ## are going to be straight-up numbers.
      expr$offset[[i]]
    } else if (expr$is_array[[i]] && i > 1L && expr$is_array[[i - 1L]]) {
      call("+", as.name(expr$order[[i - 1L]]), as.name(expr$len[[i - 1L]]))
    } else {
      stop("odin bug") # nocov
    }
  }
  expr$access <- lapply(seq_len(expr$n), delay_offset)

  x$delay <- list(time = x$rhs$value_time,
                  expr = expr,
                  default = x$rhs$value_default)
  x
}

odin_parse_delay_1_depends <- function(variables, obj) {
  ## Compute all the dependencies of this equation (and all their
  ## dependencies) filtered by those that depend on time.
  deps <- intersect(variables, names(obj$deps_rec))
  deps <- unique(c(deps, unlist(obj$deps_rec[deps], use.names=FALSE)))
  ## NOTE: We have to exclude delayed values from the dependencies
  ## here, even though they are time dependent (*different* sort
  ## of time dependence...)
  ##
  ## TODO: I don't know if delay depending on delay is tested for
  ## anywhere?
  time_name <- if (obj$info$discrete) STEP else TIME
  deps <- setdiff(deps[obj$stage[deps] == STAGE_TIME],
                  c(time_name, names_if(obj$traits[, "uses_delay"])))
  deps <- deps[order(match(deps, names(obj$deps_rec)))]

  deps_vars <- intersect(obj$vars, deps)
  deps <- setdiff(deps, deps_vars)
  ## I don't think this is really needed here?
  deps_is_array <- setNames(obj$traits[deps, "is_array"], deps)

  ## TODO: I think that it will be worthwhile flagging in some cases
  ## that *all* variables are used and then using a shortcut when
  ## generating the delay.  That's easy enough to detect because it's
  ##
  ##   all(obj$vars %in% deps)
  ##
  ## In that case we'd skip over a bunch of stuff, get the entire
  ## vector (which in the case of the discrete model is one big memcpy
  ## and so possibly more efficient).
  ret <- odin_parse_extract_order(obj, subset = deps_vars)
  ret$deps <- deps
  ret$deps_is_array <- deps_is_array
  ret
}
