## Combine array expressions into a single set of expressions.  This
## means that the pair:
##
##   x[1] <- ...
##   x[2:n] <- ...
##
## will get grouped together as a single x.  All dependencies of the
## expression will be combined, and the source reference also gets
## updated.
##
## This is (potentially) the biggest set of changes that happens to
## the set of equations, and coming out of this function we will
## (potentially) have reordered and resized the eqs, traits and
## names_target elements.
odin_parse_arrays <- function(obj) {
  ## Set the type (int/double) based on variable usage as array
  ## indices.
  obj <- odin_parse_arrays_set_type(obj)

  ## Then check that everything is used appropriately
  odin_parse_arrays_check_usage(obj)

  ## TODO: it's possible that this will allow through arrays that are
  ## defined (i.e. have a dim() call) but are never used.  This should
  ## probably be an error.

  ## Now, work through the dim() calls so we establish dimensionality
  ## of arrays.
  nd <- odin_parse_arrays_nd(obj)
  ## And push these back into the dim() calls (which is the names on
  ## the output here).
  eqs <- obj$eqs
  for (i in names(nd)) {
    eqs[[i]]$nd <- nd[[i]]
    eqs[[i]]$lhs$nd <- NULL # cleanup a little, but perhaps leave here?
  }
  obj$eqs <- eqs

  ## Then, work out which sets to combine
  is_array <- obj$traits[, "is_array"]
  if (any(is_array)) {
    i <- match(names(eqs), unique(names(eqs)[is_array]))
    i <- unname(split(which(!is.na(i)), na.omit(i)))
    i_repl <- viapply(i, "[[", 1L)
    eqs[i_repl] <- lapply(i, odin_parse_arrays_1, obj)
    ## Drop the duplicated lines (perhaps)
    i_drop <- unlist(lapply(i, "[", -1L))
    if (length(i_drop) == 0L) {
      obj$eqs <- eqs
    } else {
      obj$eqs <- eqs[-i_drop]
      obj$traits <- obj$traits[-i_drop, , drop=FALSE]
      obj$names_target <- obj$names_target[-i_drop]
    }
  }

  ## Bunch of checking now everything is tidied up; this mostly checks
  ## that where used arrays have the correct dimensions.
  odin_parse_array_check(obj)

  obj
}

odin_parse_arrays_set_type <- function(obj) {
  ## Need to identify calls to length and dim return integers.  This
  ## could probably be extended a little bit to pick up on cases where
  ## the calls are dim() and length() calls joined by arithmetic
  ## operators (except '/')
  is_rhs_dim <- function(x) {
    is_call(x$rhs$value, quote(dim)) || is_call(x$rhs$value, quote(length))
  }

  is_dim <- obj$traits[, "is_dim"]
  is_array <- obj$traits[, "is_array"]
  index_vars <- unique(unlist(c(
    lapply(obj$eqs[is_dim], function(x) x$rhs$depends$variables),
    lapply(obj$eqs[is_array], function(x) x$lhs$depends$variables),
    names_if(vlapply(obj$eqs, is_rhs_dim)))))

  ## TODO: There are actually times where this might make sense,
  ## especially when applied in a conditional.  Now that array size is
  ## static(ish) this should be OK...
  if (TIME %in% index_vars) {
    i <- which(is_array)[vlapply(obj$eqs[is_array], function(x)
      any(TIME %in% x$lhs$depends$variables))]
    odin_error("Array indices may not be time",
               get_lines(obj$eqs[i]), get_exprs(obj$eqs[i]))
  }

  ## Determine which variables are array extents and indices; we'll
  ## flag these as integers.  At the same time we need to try and work
  ## out which of these are confusing (perhaps used as an argument to
  ## division).
  err <- intersect(index_vars, names_if(is_array))
  if (length(err) > 0L) {
    i <- which(is_array)[vlapply(obj$eqs[is_array], function(x)
      any(err %in% x$lhs$depends$variables))]
    odin_error(sprintf("Array indices may not be arrays (%s used)",
                       pastec(err)),
               get_lines(obj$eqs[i]), get_exprs(obj$eqs[i]))
  }

  ## Set a data_type element (on the lhs) to int for all variables
  ## that are used as indices.
  types <- ifelse(obj$names_target %in% index_vars, "int", "double")
  for (i in seq_along(obj$eqs)) {
    obj$eqs[[i]]$lhs$data_type <- types[[i]]
  }

  obj
}

## Called for error checking only; no modifications to object:
odin_parse_arrays_check_usage <- function(obj) {
  is_dim <- obj$traits[, "is_dim"]
  is_array <- obj$traits[, "is_array"]
  names_target <- obj$names_target

  ## First, check that every variable that is an array is always
  ## assigned as an array:
  err <- !(is_array | is_dim) & names_target %in% obj$names_target[is_dim]
  if (any(err)) {
    odin_error(sprintf("Array variables must always assign as arrays (%s)",
                       paste(unique(names_target[err]), collapse=", ")),
               get_lines(obj$eqs[err]), get_exprs(obj$eqs[err]))
  }

  ## Then, start checking for duplicates:
  err <- is_duplicated(names(obj$eqs)) & !is_array
  if (any(err)) {
    odin_error(sprintf("Duplicate entries must all be array assignments (%s)",
                       paste(unique(names_target[err]), collapse=", ")),
               get_lines(obj$eqs[err]), get_exprs(obj$eqs[err]))
  }

  ## Prevent:
  ##   x[] <- user()
  ##   x[1] <- 1
  err <- is_duplicated(names(obj$eqs)) & is_array & obj$traits[, "uses_user"]
  if (any(err)) {
    odin_error(sprintf("Duplicate entries may not use user() (%s)",
                       paste(unique(names_target[err]), collapse=", ")),
               get_lines(obj$eqs[err]), get_exprs(obj$eqs[err]))
  }

  name_dim <- vcapply(obj$eqs[is_array], function(x) x$lhs$name_dim)
  err <- which(is_array)[!(name_dim %in% names(obj$eqs))]
  if (length(err) > 0L) {
    odin_error(sprintf("Missing dim() call for %s, assigned as an array",
                       paste(unique(names_target[err]), collapse=", ")),
               get_lines(obj$eqs[err]), get_exprs(obj$eqs[err]))
  }
}

odin_parse_arrays_nd <- function(obj) {
  eqs <- obj$eqs
  is_dim <- obj$traits[, "is_dim"]

  nd <- viapply(eqs[is_dim], function(x) x$lhs$nd)
  nd_user <- nd == DIM_USER
  nd_dep  <- nd == DIM_DEPENDENT
  if (any(nd_user)) {
    ## Then, check for user-driven array sizes; we'll pull nd from the
    ## definition of the variable; it should come in a pair like:
    ##   x[] <- user()
    ##   dim(x) <- user()
    ## and we're after nd from this.
    i <- match(obj$names_target[is_dim][nd_user], names(eqs))

    err <- !obj$traits[i, "uses_user"]
    if (any(err)) {
      odin_error("user-specified dim() must be used with user-specified array",
                 get_lines(eqs[i][err]), get_exprs(eqs[i][err]))
    }

    err <- !obj$traits[i, "is_array"]
    if (any(err)) {
      odin_error("user-specified dim() must be used with array",
                 get_lines(eqs[i][err]), get_exprs(eqs[i][err]))
    }

    nd[nd_user] <- viapply(eqs[i], function(x) x$lhs$nd)
  }
  if (any(nd_dep)) {
    i <- match(obj$names_target[is_dim][nd_dep], names(eqs))
    nd[nd_dep] <- viapply(eqs[i], function(x) x$lhs$nd)
  }

  ## Now we have the nd, some checks to make sure it makes sense.
  for (x in eqs[obj$traits[, "is_array"]]) {
    nd_x <- nd[[x$lhs$name_dim]]
    if (x$lhs$nd != nd_x) {
      odin_error(
        sprintf("Array dimensionality is not consistent (expected %d %s)",
                nd_x, ngettext(abs(nd_x), "index", "indices")),
        x$line, x$expr)
    }
  }

  nd
}

odin_parse_arrays_1 <- function(idx, obj) {
  eqs <- obj$eqs
  x <- eqs[[idx[[1L]]]]

  x$depends <- join_deps(lapply(eqs[idx], function(x) x[["depends"]]))
  x$expr <- lapply(eqs[idx], "[[", "expr")
  x$line <- viapply(eqs[idx], "[[", "line")

  ## TODO: some of the lhs depends stuff will not matter so much now.
  x$lhs$index <- lapply(eqs[idx], function(x) x[["lhs"]][["index"]])
  x$lhs$depends <-
    join_deps(lapply(eqs[idx], function(x) x[["lhs"]][["depends"]]))

  x$rhs$type <- vcapply(eqs[idx], function(x) x[["rhs"]][["type"]])
  x$rhs$depends <-
    join_deps(lapply(eqs[idx], function(x) x[["rhs"]][["depends"]]))
  x$rhs$value <- lapply(eqs[idx], function(x) x[["rhs"]][["value"]])

  ## TODO: All these sanity checks need major overhauls, I think;
  ## these are just here to make sure that we hit all the

  ## Sanity check (needs overhaul I think).  The stopifnot ones guard
  ## against implementation details changing.
  ok <- c("name", "lhs", "rhs", "depends", "expr", "line")
  stopifnot(length(setdiff(unlist(lapply(eqs[idx], names)), ok)) == 0L)
  ## NOTE: mixed type specials are dealt with elsewhere.  By this I
  ## mean that a variable is more than one of initial(), deriv(),
  ## output() and plain.
  used_lhs <- unlist(lapply(eqs[idx], function(x) names(x$lhs)))
  ok <- c("type", "name", "name_target", "index", "nd", "name_dim",
          "depends", "special", "data_type")
  stopifnot(length(setdiff(used_lhs, ok)) == 0L)

  ## NOTE: These *could* have been done with traits, but are not.  I
  ## think this way around makes sense.
  used_rhs <- unlist(lapply(eqs[idx], function(x) names(x$rhs)))
  single <- c("delay", "user", "interpolate")
  if (any(single %in% used_rhs)) {
    if (length(idx) > 1L) {
      f <- paste0(intersect(single, used_rhs), "()", collapse=", ")
      odin_error(sprintf(
        "%s may only be used on a single-line array assignment", f),
        get_lines(eqs[idx]), get_exprs(eqs[idx]))
    }
  } else if (identical(x$lhs$special, "output") && length(idx) > 1L) {
    output_self <- vlapply(eqs[idx], function(x) isTRUE(x$rhs$output_self))
    if (any(output_self)) {
      odin_error(sprintf(
        "Direct output of %s only allowed on single-line array assignment",
        x$lhs$name_target),
        get_lines(eqs[idx]), get_exprs(eqs[idx]))
    }
  } else {
    ok <- c("type", "depends", "value", "user", "default", "sum", "output_self")
    stopifnot(length(setdiff(used_rhs, ok)) == 0L)
  }

  ## Array delay variables need to delay on the dimensions of their
  ## "present" array, so that the order of initialisation is always
  ## correct.  In practice I don't think this is a big deal because
  ## array sizing is not time dependent.  However, this resolves a
  ## difficulty in determining the total array size of the delay
  ## bookkeeping indices.
  if (isTRUE(x$rhs$delay)) {
    d <- setdiff(x$rhs$depends_delay$variables, c(obj$vars, INDEX))
    extra <- names(which(obj$traits[d, "is_array"]))
    if (length(extra) > 0L) {
      x$rhs$depends$variables <- union(x$rhs$depends$variables,
                                       array_dim_name(extra))
    }
  }

  x
}

## This is *entirely* checking things; no modification to the object
## itself here.
##
## It might be nice to merge some of this with
## odin_parse_arrays_check_usage, but for this one we need to know nd,
## which needs the arrays merged.
odin_parse_array_check <- function(obj) {
  ## Next, check all the lhs array variables are OK; these must be
  ## constant or user (not time based).  At the same time it would be
  ## great to work out how large the arrays actually are; looking for a
  ## vector of dim here for all of them.  This might be good to go
  ## into another function.
  ##
  ## - if deriv is array, initial must be and they must have the same
  ##   computed size.
  ## - all assignments into array varibles are checked already
  ## - all references *from* array variables must be indexed
  ##   - those references should be range-checked where possible
  is_array <- obj$traits[, "is_array"]
  is_dim <- obj$traits[, "is_dim"]

  ## Number of dimensions for each variable array variable.
  nd <- viapply(obj$eqs[is_array], function(x) x$lhs$nd)
  ## TODO: what about is_initial here?
  i <- obj$traits[names(nd), "is_deriv"]
  names(nd)[i] <- obj$names_target[is_array][i]

  ## need to check all length and dim calls here.  Basically we're
  ## looking for length() to be used with calls on nd==1 arrays and
  ## range check all dim() calls on the others.  This is moderately
  ## complicated and we'll need to poke into some expressions we've
  ## looked at already.  Later on an optimisation pass we can try
  ## precompute the required information but that's just asking for
  ## headaches at the moment.  We'll need to check that on both the
  ## lhs and rhs for every expression that uses either.
  ##
  ## At the same time, we check to make sure that all rhs uses of the
  ## arrays are OK too.
  for (eq in obj$eqs) {
    uses_dim <-
      any(c("dim", "length") %in% eq$lhs$depends$functions) ||
      any(c("dim", "length") %in% eq$depends$functions)
    if (uses_dim) {
      odin_parse_arrays_check_dim(eq, nd)
    }
  }

  nms_arrays <- unique(obj$names_target[is_array])
  for (eq in obj$eqs) {
    uses_array <-
      any(eq$depends$variables %in% nms_arrays) ||
      any(eq$depends$functions %in% "[") ||
      any(eq$rhs$depends_delay$variables %in% nms_arrays) ||
      any(eq$rhs$depends_delay$functions %in% "[")
    ## Special case for assignments of the form:
    ##
    ##   output(foo) <- foo
    ##   output(foo) <- TRUE
    ##
    ## which will be checked elsewhere (and ignored)
    uses_array <- uses_array & !isTRUE(eq$rhs$output_self)
    if (uses_array) {
      eq_expr <- as.expression(eq$expr)
      if (isTRUE(eq$rhs$delay)) {
        odin_parse_arrays_check_rhs(eq$rhs$value_expr, nd, eq$line,
                                    as.expression(eq$expr))
        odin_parse_arrays_check_rhs(eq$rhs$value_time, nd, eq$line,
                                    as.expression(eq$expr))
      } else {
        odin_parse_arrays_check_rhs(eq$rhs$value, nd, eq$line,
                                    as.expression(eq$expr))
      }
    }
  }

  obj
}

odin_parse_arrays_check_dim <- function(x, nd) {
  ## Now, we need to collect and check all usages of length and check.
  ## If we extract all usages we can check them.
  check <- function(x, throw) {
    if (is.recursive(x)) {
      call <- x[[1L]]
      if (identical(call, quote(length))) {
        if (length(x) != 2L) {
          throw("length() requires exactly one argument (recieved %d)",
                  length(x) - 1L)
        } else if (!is.symbol(x[[2L]])) {
          throw("argument to length must be a symbol")
        } else {
          nm <- as.character(x[[2L]])
          if (!(nm %in% names(nd))) {
            throw("argument to length must be an array (%s is not)", nm)
          } else if (nd[[nm]] != 1L) {
            throw("argument to length must be a 1-D array (%s is %d-D)",
                    nm, nd[[nm]])
          }
        }
      } else if (identical(call, quote(dim))) {
        if (length(x) != 3L) {
          throw("dim() requires exactly two arguments (recieved %d)",
                  length(x) - 1L)
        } else if (!is.symbol(x[[2L]])) {
          throw("first argument to dim must be a symbol")
        } else {
          nm <- as.character(x[[2L]])
          if (!(nm %in% names(nd))) {
            throw("first argument to dim must be an array (%s is not)", nm)
          } else if (nd[[nm]] == 1L) {
            throw("dim() must not be used for 1D arrays (use length)")
          } else if (!is_integer_like(x[[3L]])) {
            throw("second argument to dim() must be an integer")
          } else if (x[[3L]] < 1 || x[[3L]] > nd[[nm]]) {
            throw("array index out of bounds, must be one of 1:%d", nd[[nm]])
          }
        }
      } else {
        lapply(x[-1L], check, throw)
      }
    }
    NULL
  }
  make_throw <- function(line, expr) {
    force(line)
    force(expr)
    function(fmt, ...) {
      odin_error(sprintf(fmt, ...), line, expr)
    }
  }

  if (x$lhs$type == "array") {
    for (i in seq_along(x$expr)) {
      check(x$expr[[i]], make_throw(x$line[[i]], x$expr[[i]]))
    }
  } else {
    check(x$expr, make_throw(x$line, x$expr))
  }
}

odin_parse_arrays_check_rhs <- function(rhs, nd, line, expr) {
  if (is.list(rhs)) {
    expr <- as.expression(expr)
    for (i in seq_along(rhs)) {
      odin_parse_arrays_check_rhs(rhs[[i]], nd, line[[i]], expr[[i]])
    }
  }
  nms <- names(nd)
  throw <- function(...) {
    odin_error(sprintf(...), line, expr)
  }

  check <- function(e, special) {
    if (!is.recursive(e)) { # leaf
      if (!is.symbol(e)) { # A literal of some type
        return()
      } else if (is.null(special) && deparse(e) %in% nms) {
        throw("Array '%s' used without array index", deparse(e))
      }
    } else if (is.symbol(e[[1L]])) {
      f_nm <- as.character(e[[1L]])
      if (identical(f_nm, "[")) {
        x <- deparse(e[[2L]])
        if (!is.null(special)) {
          throw(
            "Within special function %s, array %s must be used without '['",
            special, x)
        }
        ijk <- as.list(e[-(1:2)])
        if (x %in% nms) {
          if (length(ijk) != nd[[x]]) {
            throw(
              "Incorrect dimensionality for %s in '%s' (expected %d)",
              x, deparse_str(e), nd[[x]])
          }
          sym <- find_symbols(ijk)
          nok <- setdiff(sym$functions, VALID_ARRAY)
          if (length(nok) > 0L) {
            throw(
              "Disallowed functions used for %s in '%s': %s",
              x, deparse_str(e), pastec(nok))
          }
          nok <- intersect(sym$variables, nms)
          if (length(nok) > 0L) {
            throw("Disallowed variables used for %s in '%s': %s",
                  x, deparse_str(e), pastec(nok))
          }
          if ("" %in% sym$variables) {
            throw("Empty array index not allowed on rhs")
          }
        } else {
          throw("Unknown array variable %s in '%s'", x, deparse_str(e))
        }
      } else {
        if (f_nm %in% c("sum", "length", "dim", "interpolate")) {
          special <- f_nm
          if (length(e) < 2L) {
            throw("Special function %s requires at least 1 argument", special)
          } else {
            if (!(deparse(e[[2L]]) %in% nms)) {
              throw("Special function %s requires array as first argument",
                    special)
              ## Don't proceed any further at this point, as we can
              ## hit generated code, especially when the function is
              ## 'sum' as by this point it has been expanded by
              ## rewrite_sum.
              return()
            }
          }
        } else {
          special <- NULL
        }
        for (a in as.list(e[-1])) {
          if (!missing(a)) {
            check(a, special)
          }
        }
      }
    }
  }

  check(rhs, NULL)
  invisible(NULL) # never return anything at all.
}
