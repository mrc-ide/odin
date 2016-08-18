## All the code that starts odin_parse_expr is used to parse an
## individual expression line, without reference to any other line.
odin_parse_exprs <- function(exprs) {
  ## TODO: This will eventually run with some sort of error collection
  ## step so that all the errors are reported at once within this
  ## block.  Similar approaches will apply elsewhere.  Once that
  ## happens, the expression bit can roll in I think.
  lines <- utils::getSrcLocation(exprs, "line")
  lapply(seq_along(exprs), function(i) odin_parse_expr(exprs[[i]], lines[[i]]))
}

odin_parse_expr <- function(expr, line) {
  lhs <- odin_parse_expr_lhs(expr[[2L]], line, expr)
  rhs <- odin_parse_expr_rhs(expr[[3L]], line, expr)
  deps <- join_deps(list(lhs$depends, rhs$depends))

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

  list(name=lhs$name,
       lhs=lhs,
       rhs=rhs,
       depends=deps,
       expr=expr,
       line=line %||% NA_integer_)
}

odin_parse_expr_lhs <- function(lhs, line, expr) {
  if (is.name(lhs)) {
    ret <- list(type="symbol", name=deparse(lhs))
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
    re <- sprintf("^(%s)_.*", paste(RESERVED_PREFIX, collapse="|"))
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
  if (nd > 3L) {
    odin_error(
      sprintf("Arrays must have at at most 3 dimensions (given %d)", nd),
      line, as.expression(expr))
  }

  is_empty <- vlapply(index, identical, quote(expr=))
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
    extent_max <- lapply(tmp, attr, "value_max", exact=TRUE)
    extent_min <- lapply(tmp, attr, "value_min", exact=TRUE)
    is_range <- !vlapply(extent_min, is.null)
  } else {
    msg <- paste0("\t\t", vcapply(tmp[!ok], attr, "message"), collapse="\n")
    odin_error(sprintf("Invalid array use on lhs:\n%s", msg),
               line, expr)
  }

  deps <- find_symbols(index)
  err <- intersect(INDEX, deps$variables)
  if (length(err) > 0L) {
    odin_error(
      sprintf("Special index variable %s may not be used on array lhs",
              pastec(err)), line, as.expression(expr))
  }

  ## Build a big data structure out of all the index stuff; it's
  ## going to be heaps easier to deal with later.
  idx <- list(value=index,
              is_range=is_range,
              extent_max=extent_max,
              extent_min=extent_min)

  list(type="array",
       name=deparse(lhs[[2L]]),
       index=idx,
       nd=nd,
       depends=deps)
}

odin_parse_expr_lhs_special <- function(lhs, line, expr) {
  if (length(lhs) != 2L) {
    odin_error("Invalid length special function on lhs", line, expr)
  }
  fun <- deparse_str(lhs[[1L]])
  if (any(find_symbols(lhs[[2L]])$functions %in% SPECIAL_LHS)) {
    odin_error("lhs functions require exactly one argument", line, expr)
  }
  ret <- odin_parse_expr_lhs(lhs[[2L]], line, expr)
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
    ret <- list(type="atomic", value=rhs)
  } else if (is.call(rhs) || is.name(rhs)) {
    deps <- find_symbols(rhs)
    err <- intersect(SPECIAL_LHS, deps$functions)
    if (length(err) > 0L) {
      odin_error(sprintf("Function %s is disallowed on rhs",
                         paste(unique(err), collapse=", ")), line, expr)
    }
    if ("delay" %in% deps$functions) {
      ret <- odin_parse_expr_rhs_delay(rhs, line, expr)
    } else if ("user" %in% deps$functions) {
      ret <- odin_parse_expr_rhs_user(rhs, line, expr)
    } else if ("interpolate" %in% deps$functions) {
      ret <- odin_parse_expr_rhs_interpolate(rhs, line, expr)
    } else if ("sum" %in% deps$functions) {
      ret <- odin_parse_expr_rhs_sum(rhs, line, expr)
    } else {
      ret <- list(type="expression",
                  depends=deps,
                  value=rhs)
    }
    ## NOTE: _not_ exclusive conditions
    if ("if" %in% deps$functions) {
      odin_parse_expr_rhs_check_if(rhs, line, expr)
    }
  } else {
    odin_error("Unhandled expression on rhs", line, expr)
  }
  ret
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
    odin_error("delay() must surround entire rhs", line, expr)
  }
  if (length(rhs) != 3L) {
    odin_error("delay() requires exactly two arguments", line, expr)
  }
  deps_delay_expr <- find_symbols(rhs[[2L]])
  deps_delay_time <- find_symbols(rhs[[3L]])
  fns <- c(deps_delay_expr$functions, deps_delay_time$functions)
  if ("delay" %in% fns) {
    odin_error("delay() may not be nested", line, expr)
  }
  if (TIME %in% deps_delay_expr) {
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
    odin_error("delay() may not refer to time as that's confusing")
  }
  list(type="expression",
       delay=TRUE,
       ## resolved at the same time as everything else:
       depends=deps_delay_time,
       ## resolved independently in the previous time:
       depends_delay=deps_delay_expr,
       value_expr=rhs[[2L]],
       value_time=rhs[[3L]])
}

odin_parse_expr_rhs_user <- function(rhs, line, expr) {
  if (!identical(rhs[[1L]], quote(user))) {
    odin_error("user() must be the only call on the rhs", line, expr)
  }
  if (length(rhs) > 2L) {
    odin_error("user() call must have zero or one argument", line, expr)
  }

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
  default <- length(rhs) == 2L
  ret <- list(type="expression",
              depends=deps,
              value=if (default) rhs[[2L]] else NULL,
              default=default,
              user=TRUE)
}

odin_parse_expr_rhs_interpolate <- function(rhs, line, expr) {
  if (!is_call(rhs, quote(interpolate))) {
    stop("interpolate can only be used as a top level expression")
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
        paste(INTERPOLATION_TYPES, collapse=", ")),
        line, expr)
    }
  } else if (nargs == 2L) {
    rhs[[4L]] <- "spline"
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

  list(type="expression",
       depends=find_symbols(rhs),
       value=rhs,
       interpolate=TRUE)
}

## NOTE: The sum() calls aren't real; they are translated at this
## point (though it could just as easily be later but it's pretty
## straightforward to do it here) into calls that we can actually use.
## We don't complete the rewrite here, but instead collect all the
## appropriate arguments.  We'll do the function name rewrite (from
## sum to one of odin_sum1, odin_sum2 or odin_sum3 in the rewrite
## function when we tackle the minus1 from indices.
odin_parse_expr_rhs_sum <- function(rhs, line, expr) {
  ## TODO: It would be so much nicer if by the time this rolls
  ## around we could have already determined the number of
  ## dimensions that a sum has.  We'll pick that up later in the
  ## checks I think but the error is going to be confusing
  ## because it will be a rewritten statement.

  ## TODO: Remove this recursive approach; it should be quite a bit
  ## easier than this...
  rewrite_sum <- function(x, is_sum=FALSE) {
    if (!is.recursive(x)) {
      x
    } else {
      if (is_sum) {
        if (!is_call(x, quote(`[`))) {
          odin_error("Argument to sum must be an array index", line, expr)
        }
        x <- odin_parse_expr_rhs_replace_empty_index(x)
        tmp <- lapply(as.list(x[-(1:2)]), odin_parse_expr_lhs_check_index)
        ok <- vlapply(tmp, as.logical)
        if (all(ok)) {
          f <- function(x) {
            min <- attr(x, "value_min")
            max <- attr(x, "value_max")
            list(if (is.null(min)) max else min, max)
          }
          ret <- c(list(x[[2L]]), unlist(lapply(tmp, f), FALSE))
        } else {
          msg <- paste0("\t\t", vcapply(tmp[!ok], attr, "message"),
                        collapse="\n")
          odin_error(sprintf("Invalid array use in sum():\n%s", msg),
                     line, expr)
        }
        ret
      } else if (is_call(x, quote(sum))) {
        ## TODO: I don't know that we check the variables here are
        ## actually arrays.
        if (length(x) != 2L) {
          odin_error(
            sprintf("sum() requires exactly one argument (recieved %d)",
                    length(x) - 1L), line, expr)
        }
        if (is.symbol(x[[2L]])) {
          ## sum(foo)
          ret <- call("sum", x[[2L]], 1, call("length", x[[2L]]))
        } else {
          ## TODO: replace this recursive version with something better?
          ## sum(foo[1
          args <- rewrite_sum(x[[2L]], TRUE)
          n <- (length(args) - 1L) / 2L
          if (n > 1) {
            args <- c(args,
                      call("dim", args[[1L]], 1),
                      if (n > 2) call("dim", args[[1L]], 2))
          }
          ret <- as.call(c(list(quote(sum)), args))
        }
        ret
      } else {
        args <- lapply(as.list(x[-1L]), rewrite_sum, FALSE)
        as.call(c(list(x[[1L]]), args))
      }
    }
  }

  rhs <- rewrite_sum(rhs)
  list(type="expression",
       depends=find_symbols(rhs),
       value=rhs,
       sum=TRUE)
}

######################################################################

## Support functions below here

odin_parse_expr_rhs_check_if <- function(rhs, line, expr) {
  ## Rules:
  ##   - all conditionals must have an else branch
  throw <- function(...) {
    odin_error(sprintf(...), line, expr)
  }
  check_if <- function(x) {
    if (is_call(x, quote(`if`))) {
      if (length(x) != 4L) {
        throw("All if statements must have an else clause")
      } else {
        lapply(as.list(x[-1L]), check_if)
      }
    } else if (is.recursive(x)) {
      lapply(as.list(x), check_if)
    }
    invisible(NULL)
  }
  check_if(rhs)
}

odin_parse_expr_rhs_replace_empty_index <- function(x) {
  index <- as.list(x[-(1:2)])
  is_empty <- vlapply(index, identical, quote(expr=))
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
    structure(TRUE, value_max=g(value_max), value_min=f(value_min))
  } else {
    structure(FALSE, message=x)
  }
}
