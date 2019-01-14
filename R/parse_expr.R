## All the code that starts odin_parse_expr is used to parse an
## individual expression line, without reference to any other line.
odin_parse_exprs <- function(exprs) {
  ## TODO: This will eventually run with some sort of error collection
  ## step so that all the errors are reported at once within this
  ## block.  Similar approaches will apply elsewhere.  Once that
  ## happens, the expression bit can roll in I think.
  lines <- utils::getSrcLocation(exprs, "line")
  src <- utils::getSrcref(exprs)
  if (length(exprs) == 0) {
    stop("Empty input: no expressions were provided", call. = FALSE)
  }

  ## Sensible defaults in the case of directly executed code from the
  ## command-line.
  if (is.null(lines)) {
    lines <- seq_along(exprs)
  }
  if (is.null(src)) {
    src <- vcapply(exprs, deparse_str)
  }

  ret <- lapply(seq_along(exprs), function(i)
    odin_parse_expr(exprs[[i]], lines[[i]], src[[i]]))
  names(ret) <- vcapply(ret, "[[", "name")
  ret
}

odin_parse_expr <- function(expr, line, src) {
  line <- line %||% NA_integer_
  if (is.null(src)) {
    expr_str <- deparse_str(expr)
  } else {
    expr_str <- paste(as.character(src), collapse = "\n")
  }
  ## TODO: we could use this in the errors I think and everything will
  ## more or less just work
  lhs <- odin_parse_expr_lhs(expr[[2L]], line, expr)
  rhs <- odin_parse_expr_rhs(expr[[3L]], line, expr)
  depends <- join_deps(list(lhs$depends, rhs$depends))

  ## Below here uses both the lhs and rhs:
  if (isTRUE(rhs$user) &&
      !is.null(lhs$special) && !identical(lhs$special, "dim")) {
    odin_error("user() only valid for non-special variables", line, expr)
  }
  ## This might actually be too strict because it's possible that dydt
  ## could be delayed dzdt but that seems unlikely.  Definitely cannot
  ## be most of the others.
  if (isTRUE(rhs$delay) && !is.null(lhs$special)) {
    odin_error("delay() only valid for non-special variables", line, expr)
  }

  if (identical(lhs$special, "dim")) {
    lhs$nd <- odin_parse_expr_check_dim(rhs, line, expr)
    if (lhs$nd == 1L && is_call(rhs$value, quote(c))) {
      rhs$value <- rhs$value[[2L]]
      rhs$type <- if (is.name(rhs$value)) "symbol" else "atomic"
    }
    ## This is neeeded because at this point we've dealt with 'c()'
    ## and it's not supported as an actual function.
    rhs$depends$functions <- setdiff(rhs$depends$functions, "c")
    depends$functions <- setdiff(depends$functions, "c")
  }

  if (identical(lhs$special, "output")) {
    rhs$output_self <-
      isTRUE(rhs$value) || identical(rhs$value, as.name(lhs$name_target))
    if (rhs$output_self) {
      depends$variables <- union(depends$variables, lhs$name_target)
    }
  }

  if (any(names(FUNCTIONS_INPLACE) %in% depends$functions)) {
    rhs <- odin_parse_expr_rhs_rewrite_inplace(rhs, lhs, line, expr)
  }

  ## NOTE: arrays are the only case where self referential variables
  ## are allowed.  For arrays, there's no checking here and things like
  ##   x[i] = x[i] * 2
  ## will cause a crash or nonsense behaviour.
  if (lhs$type != "array" && lhs$name %in% depends$variables) {
    odin_error("Self referencing expressions not allowed (except for arrays)",
               line, expr)
  }

  stochastic <- any(depends$functions %in% names(FUNCTIONS_STOCHASTIC))

  list(name = lhs$name,
       lhs = lhs,
       rhs = rhs,
       depends = depends,
       stochastic = stochastic,
       expr = expr,
       expr_str = expr_str,
       line = line)
}

odin_parse_expr_lhs <- function(lhs, line, expr) {
  if (is.name(lhs)) {
    ret <- list(type = "symbol", name = deparse(lhs))
  } else if (is.call(lhs)) {
    fun <- deparse_str(lhs[[1L]])
    if (fun %in% "[") { # NOTE: single indexing *only*
      ret <- odin_parse_expr_lhs_index(lhs, line, expr)
    } else if (fun %in% SPECIAL_LHS) {
      ret <- odin_parse_expr_lhs_special(lhs, line, expr)
    } else {
      odin_error(sprintf("Unhandled expression %s on lhs", fun), line, expr)
    }
  } else { # things like atomic will raise here: 1 <- 2
    odin_error("Invalid left hand side", line, expr)
  }

  ## Then a little checking of what we have been given:
  if (ret$name %in% RESERVED) {
    odin_error("Reserved name for lhs", line, expr)
  }

  if (is.null(ret$special)) {
    re <- sprintf("^(%s)_.*", paste(RESERVED_PREFIX, collapse = "|"))
    if (grepl(re, ret$name)) {
      odin_error(sprintf("Variable name cannot start with '%s_'",
                         sub(re, "\\1", ret$name)),
                 line, expr)
    }
  } else {
    ret$name_target <- ret$name
    ret$name <- sprintf("%s_%s", ret$special, ret$name)
  }

  ret
}

odin_parse_expr_lhs_index <- function(lhs, line, expr) {
  if (!is.name(lhs[[2L]])) {
    odin_error("array lhs must be a name", line, expr)
  }
  index <- as.list(lhs[-(1:2)])

  nd <- length(index)

  is_empty <- vlapply(index, identical, quote(expr = ))
  if (any(is_empty)) {
    if (length(index) == 1L) {
      index[] <- list(bquote(1:length(.(lhs[[2L]]))))
    } else {
      index[is_empty] <- lapply(as.numeric(which(is_empty)), function(i)
        bquote(1:dim(.(lhs[[2L]]), .(i))))
    }
    lhs[-(1:2)] <- index
  }

  ## Valid expressions are:
  ##
  ##   binary inline +, - (unlimited number)
  ##   A single ':' which is the only thing that generates a range
  ##   A unary -(x) is not allowed as it's too hard to control
  ##
  ## With these options, the extent of the array index can be
  ## expressed as a relatively simple expression; the min and max
  ## will be the expression with x:y substituted for x and y
  ## respectively.
  ##
  ## TODO: Consider looking for, and warning about (1:x - 1)
  ## rather than (1:x) - 1 as that will imply a negative length
  ## array.  Or we can look for the minimum value being negative.
  tmp <- lapply(index, odin_parse_expr_lhs_check_index)
  ok <- vlapply(tmp, as.logical)
  if (all(ok)) {
    extent_max <- lapply(tmp, attr, "value_max", exact = TRUE)
    extent_min <- lapply(tmp, attr, "value_min", exact = TRUE)
    is_range <- !vlapply(extent_min, is.null)
  } else {
    msg <- paste0("\t\t", vcapply(tmp[!ok], attr, "message"), collapse = "\n")
    odin_error(sprintf("Invalid array use on lhs:\n%s", msg),
               line, expr)
  }

  name <- deparse(lhs[[2L]])
  deps <- find_symbols(index)
  err <- intersect(INDEX, deps$variables)
  if (length(err) > 0L) {
    odin_error(
      sprintf("Special index variable %s may not be used on array lhs",
              pastec(err)), line, as.expression(expr))
  }

  ## The dimension for this array:
  name_dim <- array_dim_name(name)
  ## ...which must be a dependency:
  deps$variables <- union(deps$variables, name_dim)
  deps$functions <- setdiff(deps$functions, ":")

  ## Build a big data structure out of all the index stuff; it's
  ## going to be heaps easier to deal with later.
  idx <- list(value = index,
              is_range = is_range,
              extent_max = extent_max,
              extent_min = extent_min)

  list(type = "array",
       name = name,
       index = idx,
       nd = nd,
       name_dim = name_dim,
       depends = deps)
}

odin_parse_expr_lhs_special <- function(lhs, line, expr) {
  if (length(lhs) != 2L) {
    odin_error("Invalid length special function on lhs", line, expr)
  }
  fun <- deparse_str(lhs[[1L]])
  target <- lhs[[2L]]
  if (is.character(target)) {
    odin_error(sprintf("Argument to %s must be a symbol or expression", fun),
               line, expr)
  }

  if (any(find_symbols(target)$functions %in% SPECIAL_LHS)) {
    odin_error("Invalid nested lhs function usage", line, expr)
  }
  ret <- odin_parse_expr_lhs(target, line, expr)
  ret$special <- fun

  if (fun == "dim" && ret$type != "symbol") {
    odin_error("dim() must be applied to a name only (not an array)",
               line, expr)
  }

  ret
}

odin_parse_expr_rhs <- function(rhs, line, expr) {
  if (is.atomic(rhs)) {
    ## These are easy; they're constants so we can deal with these directly.
    ##
    ## Should check there that everything is of the classes: integer,
    ## logical, numeric only.  It's possible that strings would be
    ## possible but I'm not sure that's sensible.
    ret <- list(type = "atomic", value = rhs)
  } else if (is.name(rhs)) {
    ## These are easy(ish); they just can't be called a few things
    ## (probably more than what is listed here; otherwise we're going
    ## to run into trouble with things like:
    ##
    ##   foo <- `+`
    ##
    ## which is valid R but going to generally cause hell.  But these
    ## will also get picked up reasonably well later by being missing
    ## variables oin the graph.
    nm <- deparse(rhs)
    if (nm %in% c(SPECIAL_LHS, SPECIAL_RHS)) {
      odin_error(sprintf("Function '%s' is disallowed as symbol on rhs", nm),
                 line, expr)
    }
    ## TODO: consider a special 'symbol' case here?
    ret <- list(type = "expression",
                depends = list(functions = character(0),
                               variables = nm),
                value = rhs)
  } else if (is.call(rhs)) {
    fun <- deparse(rhs[[1L]])
    if (fun == "delay") {
      ret <- odin_parse_expr_rhs_delay(rhs, line, expr)
    } else if (fun == "user") {
      ret <- odin_parse_expr_rhs_user(rhs, line, expr)
    } else if (fun == "interpolate") {
      ret <- odin_parse_expr_rhs_interpolate(rhs, line, expr)
    } else {
      ret <- odin_parse_expr_rhs_expression(rhs, line, expr)
    }
  } else {
    odin_error("Unhandled expression on rhs [odin bug]", line, expr) # nocov
  }
  ret
}

odin_parse_expr_rhs_expression <- function(rhs, line, expr) {
  depends <- find_symbols(rhs)
  err <- intersect(setdiff(SPECIAL_LHS, "dim"), depends$functions)
  if (length(err) > 0L) {
    odin_error(sprintf("Function %s is disallowed on rhs",
                       paste(unique(err), collapse = ", ")), line, expr)
  }
  err <- intersect(SPECIAL_RHS, depends$functions)
  if (length(err) > 0L) {
    odin_error(sprintf("%s() must be the only call on the rhs", err[[1]]),
               line, expr)
  }

  odin_parse_expr_rhs_check_usage(rhs, line, expr)

  if ("sum" %in% depends$functions) {
    rhs <- odin_parse_expr_rhs_rewrite_sum(rhs, line, expr)
    depends <- find_symbols(rhs)
  }

  if (":" %in% depends$functions) {
    odin_error("Range operator ':' may not be used on rhs", line, expr)
  }

  list(type = "expression",
       depends = depends,
       value = rhs)
}

odin_parse_expr_rhs_delay <- function(rhs, line, expr) {
  ## NOTE: Some of the restrictions for delay() are the same as
  ## user() (first call, no nesting etc).  So perhaps factor those
  ## out.
  ##
  ## Bunch of special treatment here for delay, because this is
  ## dealt with in a very different way; we'll need to know the
  ## dependencies of the delayed expression and of the time.
  ## These will be treated differently depending on which bits are
  ## time sensitive.
  if (!identical(rhs[[1]], quote(delay))) {
    odin_error("[odin bug]", line, expr) # nocov
  }
  na <- length(rhs) - 1L
  if (na < 2L || na > 3L) {
    odin_error("delay() requires two or three arguments", line, expr)
  }
  deps_delay_expr <- find_symbols(rhs[[2L]])
  deps_delay_time <- find_symbols(rhs[[3L]])
  if (na == 3L) {
    default <- odin_parse_expr_rhs(rhs[[4L]], line, expr)
  } else {
    default <- NULL
  }

  fns <- c(deps_delay_expr$functions,
           deps_delay_time$functions,
           default$depends$functions)
  if ("delay" %in% fns) {
    odin_error("delay() may not be nested", line, expr)
  }
  if (TIME %in% deps_delay_expr$variables) {
    ## TODO: This could be relaxed by substituting a different
    ## value of time within the block (say t - delay).
    ##
    ## Doing that requires rewriting the expression here
    ## (substitute would be fine) because we need to replace time
    ## with something more sensible.
    ##
    ## TODO: Worse than that is if any expression *explicitly*
    ## depends on time, then it's really confusing to deal with
    ## because "time" there should probably be the original time
    ## not the delayed time (so t - delay).  Can probably just
    ## mask the variables.
    odin_error("delay() may not refer to time as that's confusing", line, expr)
  }

  time <- rhs[[3L]]
  if (is.recursive(time) && !is_call(time, quote(`(`))) {
    time <- call("(", time)
  }

  ## TODO: merge all the delay bits together into one element.
  list(type = "expression",
       delay = TRUE,
       ## resolved at the same time as everything else:
       depends = join_deps(list(deps_delay_time, default$depends)),
       ## resolved independently in the previous time:
       depends_delay = deps_delay_expr,
       value_expr = rhs[[2L]],
       value_time = time,
       value_default = default)
}

odin_parse_expr_rhs_user <- function(rhs, line, expr) {
  if (!identical(rhs[[1L]], quote(user))) {
    odin_error("[odin bug]", line, expr) # nocov
  }

  if (any(!nzchar(names(rhs)[-(1:2)]))) {
    odin_error("Only first argument to user() may be unnamed", line, expr)
  }
  ## I'm not sure about expand.dots
  m <- match.call(function(default, integer, min, max, ...) NULL, rhs, FALSE)
  extra <- m[["..."]]
  if (!is.null(extra)) {
    odin_error(sprintf("Unknown %s to user(): %s",
                       ngettext(length(extra), "argument", "arguments"),
                       paste(dquote(names(extra))), collapse = ", "),
               line, expr)
  }

  ## This looks through default, integer, min, max
  deps <- find_symbols(as.list(rhs[-1L]))
  ## TODO: This could be relaxed I think, but dealing with
  ## potential cycles is hard because they could be generated at
  ## runtime.  So for now, these values must be constants.  I
  ## don't want to relax that until it's clear enough how arrays
  ## get treated here.
  if (length(deps$functions) > 0L) {
    odin_error("user() call must not use functions", line, expr)
  }
  if (length(deps$variables) > 0L) {
    odin_error("user() call must not reference variables", line, expr)
  }
  ret <- list(type = "expression",
              depends = deps,
              value = m$default,
              default = !is.null(m$default),
              integer = m$integer,
              min = m$min,
              max = m$max,
              user = TRUE)
}

odin_parse_expr_rhs_interpolate <- function(rhs, line, expr) {
  if (!identical(rhs[[1L]], quote(interpolate))) {
    odin_error("[odin bug]", line, expr) # nocov
  }
  nargs <- length(rhs) - 1L
  if (nargs == 3L) {
    type <- rhs[[4L]]
    if (!is.character(type)) {
      odin_error("Expected a string constant for interpolation type",
                 line, expr)
    }
    if (!(type %in% INTERPOLATION_TYPES)) {
      odin_error(sprintf(
        "Invalid interpolation type; must be one: of %s",
        paste(INTERPOLATION_TYPES, collapse = ", ")),
        line, expr)
    }
  } else if (nargs == 2L) {
    type <- rhs[[4L]] <- "spline"
  } else {
    odin_error(sprintf("2 or 3 arguments expected, recieved %d", nargs),
               line, expr)
  }

  if (!is.symbol(rhs[[2L]])) {
    odin_error("interpolation time argument must be a symbol", line, expr)
  }
  if (!is.symbol(rhs[[3L]])) {
    odin_error("interpolation target argument must be a symbol", line, expr)
  }

  value <- list(type = type, t = deparse(rhs[[2L]]), y = deparse(rhs[[3L]]))

  list(type = "expression",
       depends = find_symbols(rhs),
       value = value,
       interpolate = TRUE)
}

## NOTE: The sum() calls aren't real; they are translated at this
## point (though it could just as easily be later but it's pretty
## straightforward to do it here) into calls that we can actually use.
## In the rewrite function later we also tackle the minus1 from
## indices.
odin_parse_expr_rhs_rewrite_sum <- function(rhs, line, expr) {
  ## TODO: It would be so much nicer if by the time this rolls
  ## around we could have already determined the number of
  ## dimensions that a sum has.  We'll pick that up later in the
  ## checks I think but the error is going to be confusing
  ## because it will be a rewritten statement.
  ##
  ## TODO: This really needs to move elsewhere I have decided.  We
  ## leave the sums as they are here, but deal with them in (probably)
  ## check_rhs_index or whatever it is.  That way we'll deal with all
  ## the issues throughout.
  ##
  ## For now, I'll simplify this a bit and then try and move it into
  ## the later processing?
  ##
  ## NOTE: This needs to be recursive because we're looking through
  ## all the calls here for the `sum` call, then checking that it's
  ## not calling itself.  Things like sum(a) + sum(b) are allowed.
  rewrite_sum <- function(x, is_sum = FALSE) {
    if (!is.recursive(x)) {
      x
    } else {
      if (is_sum) {
        if (!is_call(x, quote(`[`))) {
          odin_error("Argument to sum must be a symbol or indexed array",
                     line, expr)
        }
        x <- odin_parse_expr_rhs_replace_empty_index(x)
        tmp <- lapply(as.list(x[-(1:2)]), odin_parse_expr_lhs_check_index)
        ok <- vlapply(tmp, as.logical)
        if (!all(ok)) {
          msg <- paste0("\t\t", vcapply(tmp[!ok], attr, "message"),
                        collapse = "\n")
          odin_error(sprintf("Invalid array use in sum():\n%s", msg),
                     line, expr)
        }
        f <- function(x) {
          min <- attr(x, "value_min")
          max <- attr(x, "value_max")
          list(if (is.null(min)) max else min, max)
        }
        c(list(x[[2L]]), unlist(lapply(tmp, f), FALSE))
      } else if (is_call(x, quote(sum))) {
        target <- x[[2L]]
        if (is.symbol(target)) { # sum(foo)
          ret <- x
        } else if (is.recursive(target)) { # sum(foo[1, ]), etc
          args <- rewrite_sum(target, TRUE)
          n <- (length(args) - 1L) / 2L
          fn <- FUNCTIONS_SUM[n]
          ret <- as.call(c(list(as.name(fn)), args))
        } else { # sum(1), sum(NULL)
          odin_error("Argument to sum must be a symbol or indexed array",
                     line, expr)
        }
        ret
      } else {
        args <- lapply(as.list(x[-1L]), rewrite_sum, FALSE)
        as.call(c(list(x[[1L]]), args))
      }
    }
  }

  rewrite_sum(rhs)
}

######################################################################

## Support functions below here

odin_parse_expr_rhs_check_usage <- function(rhs, line, expr) {
  ## TODO: it would be nice to restrict the functions used here to
  ## exclude the stochastic ones when making a discrete time model,
  ## but that's not going to be be easy and won't work for the
  ## single-expression focus here.  So
  len <- c(FUNCTIONS,
           setNames(FUNCTIONS[FUNCTIONS_RENAME], names(FUNCTIONS_RENAME)))

  throw <- function(...) {
    odin_error(sprintf(...), line, expr)
  }

  check_usage <- function(x) {
    if (is.recursive(x)) {
      fn <- x[[1L]]
      if (!is.name(fn)) {
        throw("Cannot process statement")
      }
      nm <- deparse(fn)
      ## I can't throw this here because it causes trouble with c()
      ## and with any user function.
      ##
      ## So later we need to go through and pick these up.
      ##
      ## if (!(nm %in% names(len))) {
      ##   throw("Unsupported function '%s'", nm)
      ## }

      n <- len[[nm]]
      nargs <- length(x) - 1L

      if (length(n) > 1L) {
        if (nargs < n[[1L]] || nargs > n[[2L]]) {
          if (is.finite(n[[2L]])) {
            throw("Expected %d-%d arguments in %s call, but recieved %d",
                  n[[1L]], n[[2L]], nm, nargs)
          } else {
            throw("Expected %d or more arguments in %s call, but recieved %d",
                  n[[1L]], nm, nargs)
          }
        }
      } else if (!is.null(n) && is.finite(n)) {
        if (nargs != n) {
          if (nm == "if") {
            ## NOTE: slightly different wording here to make the
            ## problem a little clearer.
            throw("All if statements must have an else clause")
          } else {
            throw("Expected %d %s in %s call, but recieved %d",
                  n, ngettext(n, "argument", "arguments"), nm, nargs)
          }
        }
      }
      lapply(as.list(x[-1L]), check_usage)
    }
  }
  check_usage(rhs)
}

odin_parse_expr_rhs_replace_empty_index <- function(x) {
  index <- as.list(x[-(1:2)])
  is_empty <- vlapply(index, identical, quote(expr = ))
  if (any(is_empty)) {
    if (length(index) == 1L) {
      index[] <- list(bquote(1:length(.(x[[2L]]))))
    } else {
      index[is_empty] <- lapply(as.numeric(which(is_empty)), function(i)
        bquote(1:dim(.(x[[2L]]), .(i))))
    }
    x[-(1:2)] <- index
  }
  x
}

odin_parse_expr_lhs_check_index <- function(x) {
  seen <- FALSE
  err <- collector()
  valid <- setdiff(VALID_ARRAY, ":")

  f <- function(x, max) {
    if (is.recursive(x)) {
      nm <- as.character(x[[1L]])
      if (identical(nm, ":")) {
        if (seen) {
          err$add("Multiple calls to ':' are not allowed")
        } else {
          seen <<- TRUE
        }
        if (max) {
          f(x[[3L]], max)
        } else {
          f(x[[2L]], max)
        }
      } else {
        if (nm == "-" && length(x) == 2L) {
          err$add("Unary minus invalid in array calculation")
        } else if (!(nm %in% valid)) {
          err$add(paste("Invalid function in array calculation",
                        as.character(nm)))
        }
        as.call(c(list(x[[1L]]), lapply(x[-1L], f, max)))
      }
    } else {
      x
    }
  }
  g <- function(x) {
    if (is.recursive(x) && identical(x[[1]], quote(`(`))) x[[2L]] else x
  }

  value_max <- f(x, TRUE)
  if (seen) { # check minimum branch
    seen <- FALSE
    value_min <- f(x, FALSE)
  } else {
    value_min <- NULL
  }

  x <- unique(err$get())
  if (length(x) == 0L) {
    structure(TRUE, value_max = g(value_max), value_min = f(value_min))
  } else {
    structure(FALSE, message = x)
  }
}

odin_parse_expr_check_dim <- function(rhs, line, expr) {
  if (isTRUE(rhs$user)) {
    if (isTRUE(rhs$default)) {
      odin_error("Default in user dimension size not handled", line, expr)
    }
    ret <- DIM_USER
  } else if (is.symbol(rhs$value) || is.numeric(rhs$value)) {
    ret <- 1L
  } else if (is_call(rhs$value, quote(c))) {
    ## TODO: what about dim(.) <- c(1.2, 3) -- should error
    ##
    ## TODO: what about 1 + 2 -- could be OK?
    ok <- vlapply(as.list(rhs$value[-1L]), function(x)
      is.symbol(x) || is.numeric(x) || is_dim_or_length(x))
    if (!all(ok)) {
      odin_error(
        "Invalid dim() rhs; c() must contain symbols, numbers or lengths",
        line, expr)
    } else {
      ret <- length(ok)
    }
  } else if (is_dim_or_length(rhs$value)) {
    ret <- DIM_DEPENDENT
  } else {
    odin_error("Invalid dim() rhs; expected numeric, symbol, user or c()",
               line, expr)
  }
  ret
}

odin_parse_expr_rhs_rewrite_inplace <- function(rhs, lhs, line, expr) {
  rhs_expr <- rhs$value
  fn <- deparse(rhs_expr[[1]])
  depends <- join_deps(lapply(rhs_expr[-1], find_symbols))

  ## Start strict, liberalise later
  if (!(fn %in% names(FUNCTIONS_INPLACE)) || length(depends$functions) > 0L) {
    odin_error(sprintf(
      "At present, inplace function '%s' must use no functions", fn),
      line, expr)
  }
  if (lhs$type != "array") {
    odin_error(sprintf(
      "Expected an array on the lhs of inplace function '%s'", fn),
      line, expr)
  }

  info <- FUNCTIONS_INPLACE[[fn]]
  lhs_name <- as.name(lhs$name)
  rhs_expr[[info$len + 1L]] <- call("length", lhs_name)
  rhs_expr[[info$dest + 1L]] <- lhs_name
  rhs$value <- rhs_expr
  rhs$inplace <- TRUE
  rhs$inplace_type <- info$type

  rhs
}
