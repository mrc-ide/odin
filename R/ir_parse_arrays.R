## TODO: there is *heaps* of error checking still do do in here.  We
## need to at least:
##
## * do most of the checks in odin_parse_arrays_nd
## * avoid joining multiline delays etc (odin_parse_arrays_1)
## * check usage of length and dim (odin_parse_arrays_check_dim)
## * check dimension of rhs usage (odin_parse_arrays_check_rhs)
## * later on, we need to test for time dependent dimension
##   calculations - they should not be possible; that suggests that we
##   do need to keep some reference to the dimension-ness of things
##   later on here.
##
## However, not implementing these for now because they're simply
## checking that we're not going to generate bad code.  Once
## generation is working for well-behaved cases we'll work back
## through the bad cases.

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
## The dim() calls get written out as a new group of data elements;
## we'll sort that out here too.
ir_parse_arrays <- function(eqs, variables, source) {
  ir_parse_arrays_check_indices(eqs, source)
  eqs <- ir_parse_arrays_find_integers(eqs, source)

  ir_parse_arrays_check_usage(eqs, source)

  is_dim <- vlapply(eqs, function(x) identical(x$lhs$special, "dim"))
  ## TODO: this needs properly testing elsewhere, but we rely on it here.
  stopifnot(!any(duplicated(names_if(is_dim))))

  ## At this point, we should resolve the DAG of dimensions properly.
  ## Dimensions can be resolved to the possible types:
  ##
  ## * user()
  ## * expression or c that involves "Easy" things
  ## * length(x)
  ## * dim(x)
  ## * c(...) // length or c
  ##
  ## It's not clear that this will produce the best error messages and
  ## we should try to see what happens with something like:
  ##
  ## dim(a) <- dim(b)
  ## dim(b) <- dim(a)
  ##
  ## because I think that is probably not well dealt with.
  ##
  ## The approach here should cope with
  ##
  ## dim(x) <- c(dim(a), dim(b))
  f <- function(x) {
    if (x$type == "user" || !(c("length", "dim") %in% x$depends$functions)) {
      character(0)
    } else if (is_dim_or_length(x$rhs$value)) {
      if (length(x$rhs$value) != 2L || !is.name(x$rhs$value[[2]])) {
        stop("FIXME")
      }
      array_dim_name(deparse_str(x$rhs$value[[2]]))
    } else if (is_call(x$rhs$value, "c")) {
      ok <- vlapply(as.list(x$rhs$value[-1L]), function(x)
        is.symbol(x) || is.numeric(x) || is_dim_or_length(x))
      if (!all(ok)) {
        ir_odin_error(
          "Invalid dim() rhs; c() must contain symbols, numbers or lengths",
          x$source, source)
      }
      length(ok)
    } else {
      ir_odin_error(
        "Invalid dim() rhs; c() must contain symbols, numbers or lengths",
        x$source, source)
    }
  }

  deps <- lapply(eqs[is_dim], f)
  dims <- topological_order(deps)

  ## If dim() or length() is used on a non-array element this will
  ## fail.
  stopifnot(all(dims %in% names(eqs)))

  for (eq in eqs[dims]) {
    eqs <- ir_parse_arrays_collect(eq, eqs, variables, source)
  }

  for (eq in eqs[vcapply(eqs, "[[", "type") == "copy"]) {
    eqs[[eq$name]]$array <- eqs[[eq$lhs$name_data]]$array
  }

  ir_parse_array_check_usage2(eqs, source)

  eqs
}


## Just for throws
ir_parse_arrays_check_usage <- function(eqs, source) {
  is_dim <- vlapply(eqs, function(x) identical(x$lhs$special, "dim"))
  is_array <- vlapply(eqs, function(x) x$type == "expression_array")
  is_interpolate <- vlapply(eqs, function(x) x$type == "interpolate")
  is_user <- vlapply(eqs, function(x) x$type == "user")
  is_copy <- vlapply(eqs, function(x) x$type == "copy")
  is_delay <- vlapply(eqs, function(x) x$type == "delay")
  is_delay_array <- vlapply(eqs, function(x)
    x$type == "delay" && !is.null(x$lhs$index))
  name_data <- vcapply(eqs, function(x) x$lhs$name_data)

  ## First, check that every variable that is an array is always
  ## assigned as an array:
  ##
  ## TODO: is_interpolate might be too lax here - in general this
  ## check is probably not ideal in the new approach.
  err <- !(is_array | is_dim | is_user | is_copy | is_delay_array |
           is_interpolate) & name_data %in% name_data[is_dim]
  if (any(err)) {
    ir_odin_error(
      sprintf("Array variables must always assign as arrays (%s)",
              paste(unique(name_data[err]), collapse = ", ")),
      ir_get_lines(eqs[err]), source)
  }

  ## Prevent:
  ##   x[] <- user()
  ##   x[1] <- 1
  ##
  ## And similar for delays and interpolated variables.  The message
  ## for output is not as nice.
  prevent <- list("user()" = is_user,
                  "interpolate()" = is_interpolate,
                  "delay()" = is_delay,
                  "direct output" = is_copy)
  check <- is_duplicated(names(eqs))
  for (i in names(prevent)) {
    err <- check & prevent[[i]]
    if (any(err)) {
      ir_odin_error(
        sprintf("%s may only be used on a single-line array", i),
        ir_get_lines(eqs[names(eqs) %in% names_if(err)]), source)
    }
  }

  ## Then, start checking for duplicates:
  err <- is_duplicated(names(eqs)) & !is_array
  if (any(err)) {
    ir_odin_error(
      sprintf("Duplicate entries must all be array assignments (%s)",
              paste(unique(name_data[err]), collapse = ", ")),
      ir_get_lines(eqs[err]), source)
  }

  err <- setdiff(name_data[is_array], name_data[is_dim])
  if (length(err) > 0L) {
    ir_odin_error(
      sprintf("Missing dim() call for %s, assigned as an array",
              paste(unique(err), collapse = ", ")),
      ir_get_lines(eqs[name_data %in% err]), source)
  }
}


ir_parse_array_check_usage2 <- function(eqs, source) {
  ## TODO: this feels really icky, but at least gets there.  What
  ## would be nicer in the longer term perhaps is something that flags
  ## the "canonical" version of a piece of data amongst equations.
  rank <- viapply(eqs, function(x) x$array$rank %||% 0L)
  names(rank) <- vcapply(eqs, function(x) x$lhs$name_data)
  rank <- rank[rank > 0 & !duplicated(names(rank))]

  for (eq in eqs) {
    ir_odin_parse_arrays_check_dim(eq, rank, source)
  }

  nms_arrays <- names(rank)

  is_int <- vlapply(eqs, function(x) identical(x$lhs$storage_mode, "int"))
  int_arrays <- intersect(nms_arrays, vcapply(eqs[is_int], function(x)
    x$lhs$name_data))

  for (eq in eqs) {
    if (eq$type == "expression_scalar") {
      ir_parse_arrays_check_rhs(eq$rhs$value, rank, int_arrays, eq, source)
    } else if (eq$type == "expression_array") {
      for (el in eq$rhs) {
        ir_parse_arrays_check_rhs(el$value, rank, int_arrays, eq, source)
      }
    } else if (eq$type == "delay") {
      ir_parse_arrays_check_rhs(eq$rhs$value, rank, int_arrays, eq, source)
      ir_parse_arrays_check_rhs(eq$delay$default, rank, int_arrays, eq, source)
    } else {
      ir_parse_arrays_check_rhs(eq, rank, int_arrays, source)
    }
  }
}


ir_odin_parse_arrays_check_dim <- function(eq, rank, source) {
  ## Now, we need to collect and check all usages of length and check.
  ## If we extract all usages we can check them.
  throw <- function(fmt, ...) {
    ir_odin_error(sprintf(fmt, ...), eq$source, source)
  }
  check <- function(x) {
    if (is.recursive(x)) {
      call <- x[[1L]]
      if (identical(call, quote(length))) {
        if (!is.symbol(x[[2L]])) {
          throw("argument to length must be a symbol")
        } else {
          nm <- as.character(x[[2L]])
          if (!(nm %in% names(rank))) {
            throw("argument to length must be an array (%s is not)", nm)
          } else if (rank[[nm]] != 1L) {
            throw("argument to length must be a 1-D array (%s is %d-D)",
                    nm, rank[[nm]])
          }
        }
      } else if (identical(call, quote(dim))) {
        if (!is.symbol(x[[2L]])) {
          throw("first argument to dim must be a symbol")
        } else {
          nm <- as.character(x[[2L]])
          if (!(nm %in% names(rank))) {
            throw("first argument to dim must be an array (%s is not)", nm)
          } else if (rank[[nm]] == 1L) {
            throw("dim() must not be used for 1D arrays (use length)")
          } else if (!is_integer_like(x[[3L]])) {
            throw("second argument to dim() must be an integer")
          } else if (x[[3L]] < 1 || x[[3L]] > rank[[nm]]) {
            throw("array index out of bounds, must be one of 1:%d", rank[[nm]])
          }
        }
      } else {
        lapply(x[-1L], check)
      }
    }
    TRUE
  }

  ## TODO: We may need to check things like delays carefully because
  ## they have dependencies in a funny place. What would be better
  ## perhaps is if we separated out the variables from the functions
  ## that are used?  Or we can just check everything...
  uses_dim <-
    any(c("dim", "length") %in% eq$lhs$depends$functions) ||
    any(c("dim", "length") %in% eq$depends$functions)
  if (uses_dim) {
    if (eq$type == "expression_scalar") {
      check(eq$rhs$value)
    } else if (eq$type == "expression_array") {
      for (el in eq$rhs) {
        check(el$value)
        for (i in el$index) {
          check(i$value)
        }
      }
    } else if (eq$type == "delay") {
      check(eq$rhs$value)
      for (i in eq$rhs$index) {
        check(i$value)
      }
      check(eq$delay$time)
      check(eq$delay$default)
    } else if (eq$type %in% c("user", "copy", "interpolate")) {
    } else {
      stop("writeme")
    }
  }
}


## There are a couple of related things that I'm going to lump
## together here for a bit
##
## TODO: once everything is basically working, refactor this into its
## component pieces (of which there are several).
##
## The basic approach is solid I think; we start from a dim(x) call
## and check and combine all arrays assignments of x.  That has a
## wrinkle for cases where x is a variable (because we need to treat
## initial(x) and deriv(x) somewhat separately).
##
## We don't currently deal with user-sized arrays at all yet, nor
## dependent arrays (dim(x) <- dim(y); and these chains could run
## arbitrarily deep).
ir_parse_arrays_collect <- function(eq, eqs, variables, source) {
  user_dim <- eq$type == "user"
  if (user_dim) {
    if (eq$lhs$name_data %in% variables) {
      ir_odin_error(
        sprintf("Can't specify user-sized variables (for %s)",
                eq$lhs$name_data),
        eq$source, source)
    }

    ## It's an error to use
    ##   dim(foo) <- user()
    ## without specifying
    ##   foo[] <- user()
    ##   foo[,] <- user()
    ## etc.
    eq_data <- eqs[[eq$lhs$name_data]]
    if (is.null(eq_data) || eq_data$type != "user") {
      ir_odin_error(
        sprintf("No array assignment found for %s, but dim() found",
                eq$lhs$name_data),
        eq$source, source)
    }

    rank <- length(eq_data$lhs$index)
    ## - see odin_parse_arrays_nd
  } else if (is_call(eq$rhs$value, "length")) {
    parent <- deparse_str(eq$rhs$value[[2]])
    rank <- 1L
  } else if (is_call(eq$rhs$value, "dim")) {
    if (length(eq$rhs$value) == 2L) {
      parent <- deparse_str(eq$rhs$value[[2]])
      i <- vcapply(eqs, function(x) x$lhs$name_data) == parent
      rank <- viapply(eqs[i], function(x) x$array$rank)
      stopifnot(length(unique(rank)) == 1)
      rank <- rank[[1L]]
    } else if (length(eq$rhs$value) == 3L) {
      if (!is_integer_like(eq$rhs$value[[3L]])) {
        stop("invalid dim call")
      }
      rank <- 1L
    }
  } else {
    if (is.symbol(eq$rhs$value) || is.numeric(eq$rhs$value)) {
      rank <- 1L
    } else if (is_call(eq$rhs$value, "c")) {
      value <- as.list(eq$rhs$value[-1L])
      ok <- vlapply(value, function(x)
        is.symbol(x) || is.numeric(x) || is_dim_or_length(x))
      if (!all(ok)) {
        ir_odin_error(
          "Invalid dim() rhs; c() must contain symbols, numbers or lengths",
          eq$source, source)
      }
      rank <- length(ok)
      eq$depends$functions <- setdiff(eq$depends$functions, "c")
      eq$rhs$value <- if (rank == 1L) value[[1L]] else value
    } else {
      ir_odin_error("Invalid dim() rhs; expected numeric, symbol, user or c()",
                    eq$source, source)
    }
  }

  dims <- ir_parse_arrays_dims(eq, rank, variables)

  ## Eject the original dim() call and add our new equations
  eqs <- c(eqs[names(eqs) != eq$name], dims$eqs)

  i <- which(vlapply(eqs, function(x)
    x$lhs$name_data == eq$lhs$name_data &&
    x$type != "alloc" &&
    x$type != "copy"))

  if (length(i) == 0L) {
    ir_odin_error(sprintf(
      "Array variable %s is never assigned; can't work out rank",
      eq$lhs$name_data),
      eq$source, source)
  }

  rank_used <- viapply(eqs[i], function(el) length(el$lhs$index))
  if (any(rank_used != rank)) {
    ir_odin_error(
      sprintf("Array dimensionality is not consistent (expected %d %s)",
              rank, ngettext(rank, "index", "indices")),
      eq$source, source)
  }

  ## TODO: in ir_parse_arrays_check_usage we do this for user() too,
  ## but I think that this is the correct place.
  ##
  ## TODO: Rethink the early exit when refactoring.
  ##
  ## TODO: I think this is now not possible to trigger
  eq_type <- vcapply(eqs[i], "[[", "type")
  if (any(eq_type == "delay")) {
    if (length(i) != 1L) {
      ir_odin_error("Multi-line delay() equations are not possible",
                    ir_get_lines(eqs[i]), source)
    }
  }

  join <- function(i, eqs, depend_alloc = TRUE) {
    use <- unname(eqs[i])
    eq_use <- use[[1]]
    if (eq_use$type == "delay") {
      eq_use$rhs$index <- eq_use$lhs$index
    } else if (eq_use$type != "user") {
      eq_use$rhs <- lapply(use, function(x)
        list(index = x$lhs$index, value = x$rhs$value))
      if (length(use) > 1L) {
        eq_use$depends <- join_deps(lapply(use, "[[", "depends"))
        eq_use$stochastic <- any(vlapply(use, "[[", "stochastic"))
        eq_use$source <- unlist(lapply(use, "[[", "source"), FALSE, FALSE)
      }
    }
    eq_use$lhs$index <- NULL
    eq_use$array <- list(rank = rank, dimnames = dims$dimnames)
    if (user_dim) {
      eq_use$depends <- NULL
      eq_use$user$dim <- TRUE
    } else {
      extra <- c(eq$name, if (depend_alloc) dims$alloc)
      eq_use$depends$variables <- union(eq_use$depends$variables, extra)
    }
    eqs[[i[[1L]]]] <- eq_use
    if (length(i) > 1L) {
      eqs <- eqs[-i[-1L]]
    }
    eqs
  }

  if (eq$lhs$name_data %in% variables) {
    j <- vcapply(eqs[i], function(x) x$lhs$special) %in% c("deriv", "update")
    eqs <- join(i[j], eqs, FALSE)
    eqs <- join(i[!j], eqs, TRUE)
  } else {
    eqs <- join(i, eqs, TRUE)
  }

  eqs
}


ir_parse_expr_check_dim <- function(rhs, line, expr) {
  if (isTRUE(rhs$user)) {
    stop("writeme")
    if (isTRUE(rhs$default)) {
      odin_error("Default in user dimension size not handled", line, expr)
    }
    rank <- NULL
    type <- "user"
  } else if (is.symbol(rhs$value) || is.numeric(rhs$value)) {
    rank <- 1L
    type <- "given"
  } else if (is_call(rhs$value, "c")) {
    ## TODO: what about dim(.) <- c(1.2, 3) -- should error
    ##
    ## TODO: what about 1 + 2 -- could be OK?
    ok <- vlapply(as.list(rhs$value[-1L]), function(x)
      is.symbol(x) || is.numeric(x) || is_dim_or_length(x))
    if (!all(ok)) {
      odin_error(
        "Invalid dim() rhs; c() must contain symbols, numbers or lengths",
        line, expr)
    }
    rank <- length(ok)
    type <- "given"
  } else if (is_dim_or_length(rhs$value)) {
    rank <- NULL
    type <- "dependent"
  } else {
    odin_error("Invalid dim() rhs; expected numeric, symbol, user or c()",
               line, expr)
  }
  rhs$rank <- rank
  rhs$type <- type
  rhs
}


## There is a general issue here that is not yet resolved, in that
## there might be a DAG of dependencies of ranks.


ir_parse_arrays_rank <- function(eq, eqs, variables) {
  user_dim <- eq$type == "user"
  if (user_dim) {
    if (eq$lhs$name_data %in% variables) {
      odin_error(sprintf("Can't specify user-sized variables (for %s)",
                         paste(nm[err], collapse = ", ")),
                 get_lines(tmp), get_exprs(tmp))
    }

    ## It's an error to use
    ##   dim(foo) <- user()
    ## without specifying
    ##   foo[] <- user()
    ##   foo[,] <- user()
    ## etc.
    eq_data <- eqs[[eq$lhs$name_data]]
    if (eq_data$type != "user") {
      odin_error(sprintf("No array assignment found for %s, but dim() found",
                         pastec(nm[err])),
                 get_lines(tmp), get_exprs(tmp))
    }

    rank <- length(eq_data$lhs$index)
    ## - see odin_parse_arrays_nd
  } else if (is_dim_or_length(eq$rhs$value)) {
    ## I think this branch can come out now?
    ## eq$rhs$value[[2]]
    ## browser()
    ## dependent dimensions
    stop("writeme")
  } else {
    if (is.symbol(eq$rhs$value) || is.numeric(eq$rhs$value)) {
      rank <- 1L
    } else if (is_call(eq$rhs$value, "c")) {
      value <- as.list(eq$rhs$value[-1L])
      ok <- vlapply(value, function(x)
        is.symbol(x) || is.numeric(x) || is_dim_or_length(x))
      if (!all(ok)) {
        odin_error(
          "Invalid dim() rhs; c() must contain symbols, numbers or lengths",
          line, expr)
      }
      rank <- length(ok)
      eq$depends$functions <- setdiff(eq$depends$functions, "c")
      eq$rhs$value <- value
    } else {
      odin_error("Invalid dim() rhs; expected numeric, symbol, user or c()",
                 line, expr)
    }
  }
  list(user_dim = user_dim,
       rank = rank,
       eq = eq)
}


ir_parse_arrays_dims <- function(eq, rank, variables) {
  nm <- eq$lhs$name_data
  user_dim <- eq$type == "user"

  if (user_dim) {
    type <- "null"
    depends_dim <- list(functions = character(0), variables = nm)
  } else {
    type <- "expression_scalar"
    depends_dim <- eq$depends
  }

  eq_length <- list(name = eq$name,
                    type = type,
                    lhs = list(name_lhs = eq$name,
                               name_data = eq$name,
                               name_equation = eq$name,
                               storage_mode = "int"),
                    rhs = eq$rhs,
                    depends = depends_dim,
                    source = eq$source)
  dimnames <- list(length = eq_length$name, dim = NULL, mult = NULL)

  eq_dim <- eq_mult <- NULL
  if (rank > 1L) {
    f_eq_dim <- function(i) {
      d <- array_dim_name(nm, i)
      list(
        name = d,
        type = type,
        lhs = list(name_lhs = d, name_data = d, name_equation = d,
                   storage_mode = "int"),
        rhs = list(value = eq$rhs$value[[i]]),
        source = eq$source,
        depends = depends_dim)
    }
    eq_dim <- lapply(seq_len(rank), f_eq_dim)
    dimnames$dim <- vcapply(eq_dim, "[[", "name")

    ## At this point, modify how we compute total length:
    dims <- lapply(dimnames$dim, as.name)
    eq_length$rhs$value <- collapse_expr(dims, "*")
    eq_length$depends <- list(functions = character(0),
                              variables = dimnames$dim)

    ## Even more bits
    if (rank > 2L) {
      f_eq_mult <- function(i) {
        j <- seq_len(i - 1)
        d <- array_dim_name(nm, paste(j, collapse = ""))
        value <- collapse_expr(dims[j], "*")
        list(
          name = d,
          type = "expression_scalar",
          lhs = list(name_lhs = d, name_data = d, name_equation = d,
                     storage_mode = "int"),
          rhs = list(value = collapse_expr(dims[j], "*")),
          source = eq$source,
          depends = list(functions = character(0),
                         variables = dimnames$dim[j]))
      }
      eq_mult <- lapply(3:rank, f_eq_mult)
    }
    dimnames$mult <- c("", dimnames$dim[[1]], vcapply(eq_mult, "[[", "name"))
  }

  if (user_dim) {
    eq_alloc <- NULL
  } else {
    nm_alloc <- if (nm %in% variables) initial_name(nm) else nm
    eq_alloc <- list(
      name = sprintf("alloc_%s", nm_alloc),
      type = "alloc",
      source = eq$source,
      depends = list(functions = character(0), variables = eq_length$name),
      lhs = list(name_lhs = nm_alloc, name_data = nm_alloc))
  }

  eqs <- drop_null(c(list(eq_length, eq_alloc), eq_dim, eq_mult))
  names(eqs) <- vcapply(eqs, "[[", "name")

  list(eqs = eqs, dimnames = dimnames, alloc = eq_alloc$name)
}


ir_parse_expr_rhs_expression_sum <- function(rhs, line, source) {
  rewrite_sum <- function(x) {
    if (!is.recursive(x)) {
      x
    } else if (is_call(x, "sum")) {
      if (length(x) != 2L) {
        stop("sum requires two args")
      }
      target <- x[[2L]]
      if (is.name(target)) {
        return(x)
      }
      if (is_call(target, "[")) {
        ## TODO: this whole block is pulled out of the old parse code
        ## and I think can be done more reasonably given we know where
        ## we're going.  For example, the check index could work
        ## directly with empty objects
        index <- as.list(target[-(1:2)])
        target <- target[[2L]]
        is_empty <- vlapply(index, identical, quote(expr = ))
        if (any(is_empty)) {
          if (length(index) == 1L) {
            index[] <- list(bquote(1:length(.(target))))
          } else {
            index[is_empty] <- lapply(as.numeric(which(is_empty)), function(i)
              bquote(1:dim(.(target), .(i))))
          }
        }
        tmp <- lapply(index, odin_parse_expr_lhs_check_index)
        ok <- vlapply(tmp, as.logical)
        if (!all(ok)) {
          msg <- paste0("\t\t", vcapply(tmp[!ok], attr, "message"),
                        collapse = "\n")
          ir_odin_error(sprintf("Invalid array use in sum():\n%s", msg),
                        line, source)
        }
        f <- function(x) {
          min <- attr(x, "value_min")
          max <- attr(x, "value_max")
          list(if (is.null(min)) max else min, max)
        }
        as.call(c(list(quote(odin_sum), target), unlist(lapply(tmp, f))))
      } else {
        ir_odin_error("Argument to sum must be a symbol or indexed array",
                      line, source)
      }
    } else {
      x[-1L] <- lapply(x[-1L], rewrite_sum)
      x
    }
  }

  rewrite_sum(rhs)
}


ir_parse_arrays_check_indices <- function(eqs, source) {
  ## Need to identify calls to length and dim return integers.  This
  ## could probably be extended a little bit to pick up on cases where
  ## the calls are dim() and length() calls joined by arithmetic
  ## operators (except '/')

  type <- vcapply(eqs, "[[", "type")
  is_dim <- type == "dim"
  is_array <- type == "expression_array"

  index_vars <- unique(unlist(c(
    lapply(eqs[is_dim], function(x) x$rhs$depends$variables),
    lapply(eqs[is_array], function(x) x$lhs$depends$variables),
    names_if(vlapply(eqs, function(x) is_dim_or_length(x$rhs$value))))))

  ## TODO: There are actually times where this might make sense,
  ## especially when applied in a conditional.  Now that array size is
  ## static(ish) this should be OK...
  ##
  ## TODO: this is all pretty awful and could be factored out to
  ## something much more reasonable.  It should be done perhaps after
  ## we compute stage, and then we can just test for any inappropriate
  ## time.
  if (TIME %in% index_vars) {
    i <- which(is_array)[vlapply(eqs[is_array], function(x)
      any(TIME %in% x$lhs$depends$variables) ||
      any(TIME %in% x$rhs$depends$variables))]
    if (any(i)) {
      ir_odin_error("Array indices may not be time",
                    ir_get_lines(eqs[i]), source)
    }

    i <- which(is_dim)[vlapply(eqs[is_dim], function(x)
      any(TIME %in% x$lhs$depends$variables) ||
      any(TIME %in% x$rhs$depends$variables))]
    if (any(i)) {
      ir_odin_error("Array extent may not be time",
                    ir_get_lines(eqs[i]), source)
    }
  }

  ## Determine which variables are array extents and indices; we'll
  ## flag these as integers.  At the same time we need to try and work
  ## out which of these are confusing (perhaps used as an argument to
  ## division).
  err <- intersect(index_vars, names_if(is_array))
  if (length(err) > 0L) {
    i <- which(is_array)[vlapply(eqs[is_array], function(x)
      any(err %in% x$lhs$depends$variables))]
    ir_odin_error(sprintf("Array indices may not be arrays (%s used)",
                          pastec(err)),
                  ir_get_lines(eqs[i]), source)
  }
}

ir_parse_arrays_find_integers <- function(eqs, source) {
  ## Set a data_type element (on the lhs) to int for all variables
  ## that are used as indices.  Here we'll throw in the index arrays
  ## too (treated separtately for now...)
  integer_arrays <- ir_parse_arrays_used_as_index(eqs)
  ## TODO: deal with inplace varaibles here too:
  ## integer_inplace <- names_if(vlapply(eqs[is_inplace], function(x)
  ##   identical(x$rhs$inplace_type, "int")))
  ## integer_vars <- c(index_vars, integer_arrays, integer_inplace)
  integer_vars <- integer_arrays

  ## TODO: this is not ideal because it has the potential to set too
  ## many things to integers; in particular we don't want to set
  ## things like dimension calls
  ##
  # TODO: Using these things is likely to end quite badly in the C
  # code I think
  name_data <- vcapply(eqs, function(x) x$lhs$name_data)
  for (i in which(name_data %in% integer_vars)) {
    if (!identical(eqs[[i]]$lhs$special, "dim")) {
      eqs[[i]]$lhs$storage_mode <- "int"
    }
  }

  eqs
}


## Any time that we have, within a rhs index, a vector that is
## indexed, that vector should be considered implicitly an integer
## vector.  This will hopefully be fairly rare.  This is probably
## part of the API that should be considered fairly open to change.
##
## Another option will be to flag types on arrays.  I could imagine
## doing:
##
##   type(x) <- "integer"
##
## But this should do for now.  Used in set_type above
ir_parse_arrays_used_as_index <- function(eqs) {
  check <- function(e, collector, in_index = FALSE) {
    if (is.recursive(e)) {
      if (is_call(e, quote(`[`))) {
        if (in_index) {
          tmp <- e[[2L]]
          if (is.symbol(tmp)) {
            collector$add(as.character(tmp))
          }
        }
        in_index <- TRUE
      }
      lapply(as.list(e[-1]), check, collector, in_index)
    }
    NULL
  }
  check1 <- function(x, collector) {
    if ("[" %in% x$depends$functions) {
      check(x$rhs$value, collector)
    }
    NULL
  }

  ret <- collector()
  lapply(eqs, check1, ret)
  unique(ret$get())
}


ir_parse_arrays_check_rhs <- function(rhs, rank, int_arrays, eq, source) {
  throw <- function(...) {
    ir_odin_error(sprintf(...), eq$source, source)
  }

  ## TODO: check that the right number of indices are used when using sum?
  array_special_function <-
    c("sum", "odin_sum", "length", "dim", "interpolate", "rmultinom")
  nms <- names(rank)

  check <- function(e, array_special) {
    if (!is.recursive(e)) { # leaf
      if (!is.symbol(e)) { # A literal of some type
        return()
      } else if (is.null(array_special) && deparse(e) %in% nms) {
        throw("Array '%s' used without array index", deparse(e))
      }
    } else if (is.symbol(e[[1L]])) {
      f_nm <- as.character(e[[1L]])
      if (identical(f_nm, "[")) {
        x <- deparse(e[[2L]])
        ijk <- as.list(e[-(1:2)])
        if (x %in% nms) {
          if (length(ijk) != rank[[x]]) {
            throw(
              "Incorrect dimensionality for '%s' in '%s' (expected %d)",
              x, deparse_str(e), rank[[x]])
          }
          sym <- find_symbols(ijk)
          nok <- setdiff(sym$functions, VALID_ARRAY)
          if (length(nok) > 0L) {
            throw(
              "Disallowed functions used for %s in '%s': %s",
              x, deparse_str(e), pastec(nok))
          }
          nok <- intersect(sym$variables, setdiff(nms, int_arrays))
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
        if (f_nm == "rmultinom") {
          arr_idx <- 2L
        } else {
          arr_idx <- 1L
        }
        if (f_nm %in% array_special_function) {
          is_sum <- f_nm %in% FUNCTIONS_SUM
          arr <- deparse(e[[arr_idx + 1L]])
          if (!(arr %in% nms)) {
            if (is_sum) {
              f_nm <- "sum" # For better error messages, rewrite back
            }
            throw("Function '%s' requires array as argument %d", f_nm, arr_idx)
          }
          if (is_sum) {
            rank_sum <- (length(e) - 2L) / 2L
            if (length(e) != 1 && rank_sum != rank[[arr]]) {
              throw("Incorrect dimensionality for '%s' in 'sum' (expected %d)",
                    arr, rank[[arr]])
            }
          }
          array_special <- f_nm
        } else {
          array_special <- NULL
        }
        for (a in as.list(e[-1])) {
          if (!missing(a)) {
            check(a, array_special)
          }
        }
      }
    }
  }

  check(rhs, NULL)
  invisible(NULL) # never return anything at all.
}