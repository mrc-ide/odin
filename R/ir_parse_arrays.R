## TODO: there is *heaps* of error checking still do do in here.  We
## need to at least:
##
## * determine which arrays are arrays of integers (odin_parse_arrays_set_type)
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
    } else {
      stop("FIXME")
    }
  }

  deps <- lapply(eqs[is_dim], f)
  dims <- topological_order(deps)

  ## If dim() or length() is used on a non-array element this will
  ## fail.
  stopifnot(all(dims %in% names(eqs)))

  for (eq in eqs[dims]) {
    eqs <- ir_parse_arrays_collect(eq, eqs, variables)
  }

  eqs
}


## Just for throws
ir_parse_arrays_check_usage <- function(eqs, source) {
  is_dim <- vlapply(eqs, function(x) identical(x$lhs$special, "dim"))
  is_array <- vlapply(eqs, function(x) x$type == "expression_array")
  is_interpolate <- vlapply(eqs, function(x) x$type == "interpolate")
  is_user <- vlapply(eqs, function(x) x$type == "user")
  is_copy <- vlapply(eqs, function(x) x$type == "copy")
  name_data <- vcapply(eqs, function(x) x$lhs$name_data)

  ## First, check that every variable that is an array is always
  ## assigned as an array:
  ##
  ## TODO: is_interpolate might be too lax here
  err <- !(is_array | is_dim | is_user | is_copy | is_interpolate) &
    name_data %in% name_data[is_dim]
  if (any(err)) {
    browser()
    ir_odin_error(
      sprintf("Array variables must always assign as arrays (%s)",
              paste(unique(name_data[err]), collapse = ", ")),
      ir_get_lines(eqs[err]), source)
  }

  ## Then, start checking for duplicates:
  err <- is_duplicated(names(eqs)) & !is_array
  if (any(err)) {
    ir_odin_error(
      sprintf("Duplicate entries must all be array assignments (%s)",
              paste(name_data[err], collapse = ", ")),
      ir_get_lines(eqs[err]), source)
  }

  ## Prevent:
  ##   x[] <- user()
  ##   x[1] <- 1
  err <- is_duplicated(names(eqs)) & is_user
  if (any(err)) {
    ir_odin_error(
      sprintf("Duplicate entries may not use user() (%s)",
              paste(unique(names_if(err))), collapse = ", "),
      ir_get_lines(eqs[err]), source)
  }

  err <- setdiff(name_data[is_array], name_data[is_dim])
  if (length(err) > 0L) {
    ir_odin_error(
      sprintf("Missing dim() call for %s, assigned as an array",
              paste(unique(names_data[err]), collapse = ", ")),
      ir_get_lines(eqs[err]), source)
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
ir_parse_arrays_collect <- function(eq, eqs, variables) {
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
    parent <- deparse_str(eq$rhs$value[[2]])
    i <- vcapply(eqs, function(x) x$lhs$name_data) == parent
    rank <- viapply(eqs[i], function(x) x$array$rank)
    stopifnot(length(unique(rank)) == 1)
    rank <- rank[[1L]]
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

  dims <- ir_parse_arrays_dims(eq, rank, variables)

  ## Eject the original dim() call and add our new equations
  eqs <- c(eqs[names(eqs) != eq$name], dims$eqs)

  i <- which(vlapply(eqs, function(x)
    x$lhs$name_data == eq$lhs$name_data &&
    x$type != "alloc" &&
    x$type != "copy"))

  rank_used <- viapply(eqs[i], function(el) length(el$lhs$index))
  if (any(rank_used != rank)) {
    ## TODO: here, show the dim() command?
    odin_error(
      sprintf("Array dimensionality is not consistent (expected %d %s)",
              nd_x, ngettext(abs(nd_x), "index", "indices")),
      x$line, x$expr)
  }

  join <- function(i, eqs, depend_alloc = TRUE) {
    use <- unname(eqs[i])
    eq_use <- use[[1]]
    if (eq_use$type != "user") {
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
        stop("writeme")
      }
    } else {
      x[-1L] <- lapply(x[-1L], rewrite_sum)
      x
    }
  }

  rewrite_sum(rhs)
}
