## The general approach here is:
##
## 1. coerce whatever we're given into a list of assignments;
##    odin_parse_prepare
##
## 2. parse each expression into core types, along with a small amount
##    of rewriting.  This stage does all the bits that can be done to
##    each expression without reference to any other expression.
##      * odin_parse_expr (in parse_expr.R)
##        -         _expr_lhs{,_index,_special}
##        -         _expr_rhs{,_delay,_user,_interpolate,_sum}
##
## 3. compute core "traits" about every expression; these are used
##    through the processing code; odin_parse_collect_traits
##
## 4. identify *variables* (based on having an initial() & deriv() on
##    the lhs); odin_parse_find_vars
##
## 5. apply (and remove) and "config()" directives in the model;
##    odin_parse_config
##
## 6. write some expressions involving variables when computing
##    initial conditions of variables (odin_parse_rewrite_initial)
##
## 7. Combine array equations involving the same expression into a
##    single expression.
##
## 8. Determine the graph structure of the model and (from that) the
##    order in which to traverse the model.

## Read in the file and do the basic classification of all expressions.
odin_parse <- function(x, as="file") {
  ## 1. Basic preparations over the expression list:
  exprs <- odin_parse_prepare(x, as)

  ## 2. Prepare each expression:
  eqs <- odin_parse_exprs(exprs)

  ## Start building the core object:
  ret <- list(eqs=eqs,
              file=if (as == "file") x else basename(tempfile("odin", ".")))

  ## 3. Compute overall information on traits (creates elements $traits
  ## and $info):
  ret <- odin_parse_collect_traits(ret)

  ## 4. Identify all ODE variables:
  ret$vars <- odin_parse_find_vars(ret$eqs, ret$traits)

  ## Then below here everything is done via modifying the object
  ## directly.  Things may (and do) modify more than one element of
  ## the object here.  During the cleanup I'll try and work out what
  ## is going on though.

  ## 5. Strip out all the configuration options, replacing them with a
  ## "config" element in the data object.
  ret <- odin_parse_config(ret)

  ## 6. When establishing initial conditions, all uses of a variable
  ## "v" on the rhs are assumed to refer to "initial(v)" and this
  ## requires some reqwriting (can be done any time after determining
  ## "vars", and before combining arrays).
  ret <- odin_parse_rewrite_initial(ret)

  ## 7. Combine array equations involving the same expression into a
  ## single expression.  NOTE: The position if this call matters a lot
  ## because it rewrites the 'eqs' object as well as the array
  ## equations themselves.
  ret <- odin_parse_combine_arrays(ret)

  ## 8. Determine the graph structure of the model and (from that) the
  ## order in which to traverse the model.
  ret <- odin_parse_dependencies(ret)

  ## ...below here not reviewed yet...
  ret <- odin_parse_output_TEMPORARY(ret)
  ret <- odin_parse_check_array_usage(ret)
  ret <- odin_parse_variable_order(ret)
  ret <- odin_parse_initial(ret)
  ret <- odin_parse_process_interpolate(ret)
  ret <- odin_parse_delay(ret)
  ret <- odin_parse_output(ret)
  ret <- odin_parse_user(ret)

  ret
}

odin_parse_prepare <- function(x, as="file") {
  parse_expression <- function(x) {
    if (inherits(x, "{")) {
      x <- as.expression(as.list(x[-1L]))
    }
    x
  }
  expr_is_assignment <- function(x) {
    length(x) == 3L &&
      (identical(x[[1]], quote(`<-`)) || identical(x[[1]], quote(`=`)))
  }
  exprs <- switch(match.arg(as, c("file", "text", "expression")),
                  file=parse(file=x, keep.source=TRUE),
                  text=parse(text=x, keep.source=TRUE),
                  expression=parse_expression(x))
  ## First pass is to check that all operations are assignments.  No
  ## for loops, no if/else statements.
  err <- which(!vlapply(exprs, expr_is_assignment))
  if (length(err) > 0L) {
    odin_error("Every line must contain an assignment", err, exprs[err])
  }

  exprs
}

odin_parse_collect_traits <- function(obj) {
  eqs <- obj$eqs
  nms <- vcapply(eqs, "[[", "name")

  ## special lhs:
  special <- vcapply(eqs, function(x) x$lhs$special %||% "")
  is_dim <- special == "dim"
  is_deriv <- special == "deriv"
  is_initial <- special == "initial"
  is_output <- special == "output"
  is_config <- special == "config"

  ## core rhs:
  is_array <- vcapply(eqs, function(x) x$lhs$type) == "array"
  is_symbol <- vcapply(eqs, function(x) x$lhs$type) == "symbol"

  ## rhs behaviour
  uses_atomic <- vlapply(eqs, function(x) identical(x$rhs$type, "atomic"))
  uses_user <- vlapply(eqs, function(x) isTRUE(x$rhs$user))
  uses_delay <- vlapply(eqs, function(x) isTRUE(x$rhs$delay))
  uses_interpolate <- vlapply(eqs, function(x) isTRUE(x$rhs$interpolate))
  uses_sum <- vlapply(eqs, function(x) isTRUE(x$rhs$sum))

  traits <- cbind(is_dim, is_deriv, is_initial, is_output, is_config,
                  is_array, is_symbol,
                  uses_atomic, uses_user, uses_delay,
                  uses_interpolate, uses_sum)
  rownames(traits) <- names(eqs)

  obj$traits <- traits
  obj$info <- list(has_array=any(is_array), # || any(is_dim)
                   has_output=any(is_output),
                   has_user=any(uses_user),
                   has_delay=any(uses_delay),
                   has_interpolate=any(uses_interpolate),
                   has_sum=any(uses_sum))

  nms_target <- names(eqs)
  i <- is_deriv | is_initial | is_dim
  nms_target[i] <- vcapply(eqs[i], function(x) x$lhs$name_target)
  obj$names_target <- nms_target

  obj
}

## Identfying variables is straightforward; they have deriv() and
## initial() calls.  It is an error not to have both.
odin_parse_find_vars <- function(eqs, traits) {
  is_deriv <- traits[, "is_deriv"]
  is_initial <- traits[, "is_initial"]

  ## Extract the *real* name here:
  name_target <- function(x) x$lhs$name_target
  vars <- unique(vcapply(eqs[is_deriv], name_target))
  vars_initial <- unique(vcapply(eqs[is_initial], name_target))

  if (!setequal(vars, vars_initial)) {
    msg <- collector()
    msg_initial <- setdiff(vars, vars_initial)
    if (length(msg_initial) > 0L) {
      msg$add("\tin deriv() but not initial(): %s",
              paste(msg_initial, collapse=", "))
    }
    msg_vars <- setdiff(vars_initial, vars)
    if (length(msg_vars) > 0L) {
      msg$add("\tin initial() but not deriv(): %s",
              paste(msg_vars, collapse=", "))
    }
    stop("derivs() and initial() must contain same set of equations:\n",
         paste(msg$get(), collapse="\n"), call.=FALSE)
  }

  err <- names(is_deriv) %in% vars
  if (any(err)) {
    odin_error(
      sprintf("variables on lhs must be within deriv() or initial() (%s)",
              paste(intersect(vars, names(eqs)), collapse=", ")),
      get_lines(eqs[err]), get_exprs(eqs[err]))
  }

  vars
}

## TODO: consider  allowing use of  initial(x) anywhere to  access the
## initial condition.  Then we can do things like
##
##   deriv(y) = r * (y - initial(y))
##
## for exponential decay towards a stabilising force. For now it's ok,
## and this can always be achieved by assignment via a common 3rd
## parameter.
##
## NOTE: This is for handling the dependency graph when one initialial
## condition depends on another.  For example (from test-odin.R)
##
##   initial(y1) <- 1
##   initial(y2) <- y1 + v1
##
## We need to rewrite y1 -> initial(y1) -> initial_y1
##
## This depends on knowing what are *variables*, so must be after
## first pass.
odin_parse_rewrite_initial <- function(obj) {
  vars <- obj$vars
  subs <- setNames(sprintf("initial_%s", vars), vars)
  env <- as.environment(lapply(subs, as.name))

  replace <- function(x, tr) {
    i <- match(x, names(tr))
    j <- !is.na(i)
    x[j] <- tr[i][j]
    x
  }
  f <- function(x) {
    if (any(x$rhs$depends$variables %in% vars)) {
      x$rhs$value <- substitute_(x$rhs$value, env)
      x$rhs$depends$variables <- replace(x$rhs$depends$variables, subs)
      x$depends$variables <- replace(x$depends$variables, subs)
    }
    x
  }

  is_initial <- obj$traits[, "is_initial"]
  obj$eqs[is_initial] <- lapply(obj$eqs[is_initial], f)

  obj
}

odin_parse_check_array_usage <- function(obj) {
  ## This is *entirely* checking things, except that we set
  ## "index_vars" on the way out.
  eqs <- obj$eqs

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
  is_array <- vlapply(eqs, function(x) identical(x$lhs$type, "array"))
  is_dim <- vlapply(eqs, function(x) identical(x$lhs$special, "dim"))
  is_rhs_length <- vlapply(eqs, function(x) is_call(x$rhs$value, quote(length)))
  is_rhs_dim <- vlapply(eqs, function(x) is_call(x$rhs$value, quote(dim)))

  ## TODO: Compute length(arr) as a special thing and allow it to be
  ## used directly in code.  Similarly allow dim(arr, {1,2,3}), stored
  ## internaly as dim_1, dim_2, dim_3; can do that with code rewriting
  ## easily enough; make dim an allowed function in the rhs arrays and
  ## lhs if not a cyclic dependency (this is all done apart from
  ## _using_ dim).
  i <- viapply(eqs[is_dim], function(x) x$stage) == STAGE_TIME
  if (any(i)) {
    odin_error("Array extent is determined by time",
               get_lines(eqs[i]), get_exprs(eqs[i]))
  }

  ## Determine which variables are array extents and indices; we'll
  ## flag these as integers.  At the same time we need to try and work
  ## out which of these are confusing (perhaps used as an argument to
  ## division).
  index_vars <- c(lapply(eqs[is_dim], function(x) x$rhs$depends$variables),
                  lapply(eqs[is_array], function(x) x$lhs$depends$variables),
                  names(which(is_rhs_length)),
                  names(which(is_rhs_dim)))
  all_index_vars <- unique(unlist(index_vars))
  err <- intersect(all_index_vars, names(which(is_array)))
  if (length(err) > 0L) {
    i <- which(is_array)[vlapply(eqs[is_array], function(x)
      any(err %in% x$lhs$depends$variables))]
    odin_error(sprintf("Array indices may not be arrays (%s used)",
                       pastec(err)),
               get_lines(eqs[i]), get_exprs(eqs[i]))
  }

  ## TODO: There are actually times where this might make sense,
  ## especially when applied in a conditional.  Now that array size is
  ## static(ish) this should be OK...
  if (TIME %in% all_index_vars) {
    i <- which(is_array)[vlapply(eqs[is_array], function(x)
      any(TIME %in% x$lhs$depends$variables))]
    odin_error("Array indices may not be time",
               get_lines(eqs[i]), get_exprs(eqs[i]))
  }

  ## Number of dimensions for each variable array variable.
  nd <- setNames(viapply(eqs[is_array], function(x) x$lhs$nd),
                 names(which(is_array)))

  ## TODO: this is a hack; we should have array-ness of vars sorted
  ## before here.
  i <- match(intersect(names(nd), sprintf("deriv_%s", obj$vars)), names(nd))
  names(nd)[i] <- sub("^deriv_", "", names(nd)[i])

  ## need to check all length and dim calls here.  Basically we're
  ## looking for length() to be used with calls on nd==1 arrays and
  ## range check all dim() calls on the others.  This is moderately
  ## complicated and we'll need to poke into some expressions we've
  ## looked at already.  Later on an optimisation pass we can try
  ## precompute the required information but that's just asking for
  ## headaches at the moment.  We'll need to check that on both the
  ## lhs and rhs for every expression that uses either.
  to_check <- vlapply(
    eqs[is_array],
    function(x) any(c("dim", "length") %in% x$lhs$depends$functions) ||
                any(c("dim", "length") %in% x$depends$functions))

  vlapply(obj$eqs[is_array][to_check], check_array_length_dim, nd)

  ## How do we determine that these are not used as floats anywhere?
  ## The actual variables are already filtered out.
  arrays <- unique(sub("^(deriv|initial)_", "", names(which(is_array))))
  uses_array <- vlapply(eqs, function(x)
    any(x$depends$variables %in% arrays) ||
    any(x$depends$functions %in% "[") ||
    any(x$rhs$depends_delay$variables %in% arrays) ||
    any(x$rhs$depends_delay$functions %in% "["))

  for (eq in eqs[uses_array]) {
    if (isTRUE(eq$rhs$delay)) {
      check_array_rhs(eq$rhs$value_expr, nd, eq$line, as.expression(eq$expr))
      check_array_rhs(eq$rhs$value_time, nd, eq$line, as.expression(eq$expr))
    } else {
      check_array_rhs(eq$rhs$value, nd, eq$line, as.expression(eq$expr))
    }
  }

  obj$index_vars <- all_index_vars
  obj
}

odin_parse_variable_order <- function(obj) {
  ## Within these classes, is it best to put them in topological order
  ## or in lexical order?  Start with lexical order as we can always
  ## tweak that later.
  ##
  ## I think it should be sorted by stage within the arrays though,
  ## and stage added here.
  vars <- obj$vars
  vars_deriv <- paste0("deriv_", vars)
  is_array <- setNames(obj$traits[vars_deriv, "is_array"], vars)

  ord <- rep(0L, length(is_array))
  array <- setNames(rep(0L, length(is_array)), vars)
  stage <- setNames(rep(STAGE_CONSTANT, length(is_array)), vars)

  if (any(is_array)) {
    tmp <- vcapply(obj$eqs[vars_deriv], function(x) x$lhs$name_dim)
    ord[is_array] <- match(tmp, names(obj$eqs))
    ## TODO: may change to function(x) x$lhs$nd
    array[is_array] <- viapply(obj$eqs[tmp], "[[", "nd")

    stage[is_array] <- viapply(obj$eqs[tmp], function(x) x$stage)
    names(stage) <- vars

    ## Reorder to get variables ordered to scalars first
    vars <- vars[order(ord)]
    is_array <- is_array[vars]
    array <- array[vars]
  }

  offset <- setNames(as.list(seq_along(is_array) - 1L), vars)
  f <- function(i) {
    if (i == 1L) {
      0L
    } else if (!is_array[[i - 1L]]) {
      offset[[i - 1L]] + 1L
    } else if (identical(offset[[i - 1L]], 0L)) {
      as.name(paste0("dim_", vars[[i - 1L]]))
    } else {
      call("+",
           as.name(paste0("offset_", vars[[i - 1L]])),
           as.name(paste0("dim_", vars[[i - 1L]])))
    }
  }
  for (i in which(is_array)) {
    offset[[i]] <- f(i)
  }
  offset_is_var <- !vlapply(offset, is.numeric)
  offset_use <- offset
  offset_use[offset_is_var] <- sprintf("offset_%s", vars[offset_is_var])

  total <- f(length(is_array) + 1L)
  total_is_var <- !is.numeric(total)
  total_use <- if (total_is_var) "dim" else total
  total_stage <- max(stage)

  ## TODO: This is likely to expand to include "used in delay", "used
  ## in output", etc...

  ## Check whether variables are actually used in the time equations:
  used_vars <- unlist(lapply(obj$eqs, function(x)
    if (x$stage >= STAGE_TIME) x$depends$variables), use.names=FALSE)
  used <- setNames(vars %in% used_vars, vars)

  obj$variable_order <-
    list(order=vars,
         is_array=is_array, # may be dropped in favour of (array > 0)
         used=used,
         array=array,
         offset=offset,
         offset_use=offset_use,
         offset_is_var=offset_is_var,
         total=total,
         total_is_var=total_is_var,
         total_use=total_use,
         total_stage=total_stage)
  obj
}

odin_parse_output <- function(obj) {
  is_output <- which(vlapply(obj$eqs, function(x)
    identical(x$lhs$special, "output")))
  obj$has_output <- length(is_output) > 0L
  if (!obj$has_output) {
    return(obj)
  }
  ## Here we really need to know the length of these things but we
  ## don't have that yet.  Then at the beginning of the derivative
  ## function we should assign everything out of the appropriate
  ## pointer so we're going to need to store some offsets.

  ## So a new function "output_order" will return NULL or a vector of
  ## indices.  We throw a bunch more offsets into the parameter vector
  ## too I think.
  vars <- names(is_output)
  vars_target <- vcapply(obj$eqs[is_output], function(x) x$lhs$name_target)
  is_array <- vlapply(obj$eqs[is_output], function(x) x$lhs$type == "array")

  ord <- rep(0, length(is_array))
  stage <- rep(STAGE_CONSTANT, length(is_array))
  names(stage) <- vars

  if (any(is_array)) {
    tmp <- vcapply(vars[is_array], array_dim_name)
    ord[is_array] <- match(tmp, names(obj$eqs))
    stage[is_array] <- viapply(obj$eqs[tmp], function(x) x$stage)
  }
  i <- order(ord)
  vars <- vars[i]
  vars_target <- vars_target[i]
  is_array <- is_array[i]
  stage <- stage[i]

  ## TODO: This is duplicated from above and could be generalised.
  offset <- setNames(as.list(seq_along(is_array) - 1L), vars)
  f <- function(i) {
    if (i == 1L) {
      0L
    } else if (!is_array[[i - 1L]]) {
      offset[[i - 1L]] + 1L
    } else if (identical(offset[[i - 1L]], 0L)) {
      as.name(array_dim_name(vars[[i - 1L]]))
    } else {
      call("+",
           as.name(paste0("offset_", vars[[i - 1L]])),
           as.name(array_dim_name(vars[[i - 1L]])))
    }
  }
  for (i in which(is_array)) {
    offset[[i]] <- f(i)
  }

  offset_is_var <- !vlapply(offset, is.numeric)
  offset_use <- offset
  offset_use[offset_is_var] <- sprintf("offset_%s", vars[offset_is_var])

  total <- f(length(is_array) + 1L)
  total_is_var <- !is.numeric(total)
  total_use <- if (total_is_var) "dim" else total
  total_stage <- max(stage)

  obj$output_order <-
    list(order=vars_target,
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

check_array_rhs <- function(rhs, nd, line, expr) {
  if (is.list(rhs)) {
    for (i in seq_along(rhs)) {
      check_array_rhs(rhs[[i]], nd, line[[i]], expr[[i]])
    }
  }
  nms <- names(nd)
  throw <- function(...) {
    odin_error(sprintf(...), line, expr)
  }

  f <- function(e, special) {
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
            f(a, special)
          }
        }
      }
    }
  }

  f(rhs, NULL)
  invisible(NULL) # never return anything at all.
}

check_array_length_dim <- function(x, nd) {
  ## Now, we need to collect and check all usages of length and check.
  ## If we extract all usages we can check them.
  f <- function(x, throw) {
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
        lapply(x[-1L], f, throw)
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
      f(x$expr[[i]], make_throw(x$line[[i]], x$expr[[i]]))
    }
  } else {
    f(x$expr, make_throw(x$line, x$expr))
  }

  TRUE
}

## TODO: At the moment there are some corner cases here that are
## neither correctly warned about, nor correctly dealt with.  Things
## like:
##
##   y[, 1:4] <- interpolate(a, b)
##
## Will sail through but not work appropriately because interpolate is
## expected to return output that is the same dimension as 'y' (I
## think this is checked somewhere).  Worse, the rank of:
##
##   y[, 1] <- interpolate(a, b)
##
## is unclear; this could work with a vector or a 1 column matrix!
##
## Anyway, for now we skip this annoying difficulty and assume that
## the user doesn't do anything crazy with these functions.
odin_parse_process_interpolate <- function(obj) {
  if (!obj$info$has_interpolate) {
    return(obj)
  }

  check_interpolate_arg <- function(nm, rank) {
    target <- obj$eqs[[nm]]
    if (is.null(target)) {
      odin_error(sprintf("Interpolation variable %s not found", nm),
                 e$line, e$expr)
    }
    if (target$lhs$type != "array" || target$lhs$nd != rank) {
      type <- if (rank == 1L) "vector" else paste(rank, "dimensional array")
      odin_error(sprintf("Expected %s to be a %s", nm, type),
                 e$line, e$expr)
    }
  }
  process1 <- function(e) {
    rank <- if (e$lhs$type == "symbol") 0L else e$lhs$nd
    if (rank > 3) {
      ## TODO: support here requires 4d input y arrays (hard-ish) but
      ## supplying required 4d arrays is likely to be horrid anyway.
      odin_error("interpolating 3d arrays not yet supported",
                 e$line, e$expr)
    }
    if (rank == 0L) {
      value <- e$rhs$value
    } else {
      if (length(e$rhs$value) != 1L) {
        odin_error("Array interpolation requires single assignment",
                   e$line, e$expr)
      }
      value <- e$rhs$value[[1L]]
    }
    check_interpolate_arg(value$t, 1L)
    check_interpolate_arg(value$y, rank + 1L)
    value$nd <- rank
    value$ny <- if (rank == 0L) 1L else array_dim_name(value$y)
    value$nt <- array_dim_name(value$t)
    value$name <- paste0("interpolate_", e$name)

    e$rhs$value <- value
    e
  }

  is_interpolate <- obj$traits[, "uses_interpolate"]
  obj$eqs[is_interpolate] <- lapply(obj$eqs[is_interpolate], process1)

  obj
}

## We're going to need to wrap this up like testthat I think, so that
## we can catch these and group them together.  But leaving that for
## now.
odin_error <- function(msg, line, expr) {
  if (is.expression(expr)) {
    expr_str <- vcapply(expr, deparse_str)
  } else {
    expr_str <- deparse_str(expr)
  }
  str <- sprintf(ifelse(is.na(line), "%s", "%s # (line %s)"), expr_str, line)
  stop(msg, paste0("\n\t", str, collapse=""), call.=FALSE)
}

get_lines <- function(x) {
  unlist(lapply(x, "[[", "line"))
}
get_exprs <- function(x) {
  as.expression(unlist(lapply(x, "[[", "expr")))
}

recursive_dependencies <- function(order, deps, vars) {
  deps_rec <- setNames(vector("list", length(order)), order)
  for (i in order) {
    j <- as.character(unlist(deps[i]))
    deps_rec[[i]] <-
      c(j, unique(as.character(unlist(deps_rec[j], use.names=FALSE))))
  }
  deps_rec
}

is_c_identifier <- function(x) {
  ## Keyword list from:
  ## http://en.cppreference.com/w/c/keyword
  c_reserved <-
    c("auto", "break", "case", "char", "const", "continue", "default",
      "do", "double", "else", "enum", "extern", "float", "for", "goto",
      "if", "inline", "int", "long", "register", "restrict", "return",
      "short", "signed", "sizeof", "static", "struct", "switch", "typedef",
      "union", "unsigned", "void", "volatile", "while")
  grepl("[A-Za-z_][A-Za-z0-9_]*", x) & !(x %in% c_reserved)
}

## NOTE:
##
## These are all "odin constants".  Some of these we'll probably make
## user configurable at some point (time is one, the index variables
## are another).  Most of the others are not changing.
STAGE_CONSTANT <- 1L
STAGE_USER <- 2L
STAGE_TIME <- 3L
STAGE_OUTPUT <- 4L
STAGES <- c("constant", "user", "time", "output")
TIME <- "t"
STATE <- "state"
DSTATEDT <- "dstatedt"
OUTPUT <- "output"
USER <- "user"
## TODO: None of these deal with the use of these as functions (only
## variables) but that needs checking too.  Not 100% sure this is done
## on the lhs index bits.  Probably need to standardise that at some
## point.
SPECIAL_LHS <- c("initial", "deriv", "output", "dim", "config")
SPECIAL_RHS <- c("user", "interpolate", "delay")
INDEX <- c("i", "j", "k")
RESERVED <- c(INDEX, TIME, STATE, DSTATEDT, USER, SPECIAL_LHS, "delay", "dde")
RESERVED_PREFIX <- c(SPECIAL_LHS, "odin", "offset", "delay", "interpolate")
VALID_ARRAY <- c("-", "+", ":", "(", "length", "dim")
INTERPOLATION_TYPES <- c("constant", "linear", "spline")

######################################################################

odin_parse_initial <- function(obj) {
  stage <- obj$stage
  nms_initial <- names(which(obj$traits[, "is_initial"]))

  ## TODO: Special treatment is needed for time-dependent initial
  ## conditions; they get special treatment and are held to max of
  ## STAGE_USER.  However, we'll record that they are time-dependent
  ## here.
  initial_stage <- (if (obj$info$has_delay) STAGE_TIME
                    else max(c(STAGE_CONSTANT, stage[nms_initial])))
  initial <- list(stage=initial_stage)

  nms_initial_t <- names(which(stage[nms_initial] == STAGE_TIME))
  if (length(nms_initial_t) > 0L) {
    ## NOTE: The intersect() here ensures correct ordering.
    d <- unique(unlist(obj$deps_rec[nms_initial_t], use.names=FALSE))
    initial$time_deps <-
      intersect(names(obj$deps_rec), setdiff(d[stage[d] == STAGE_TIME], TIME))
  }

  obj$initial <- initial
  obj
}

odin_parse_output_TEMPORARY <- function(obj) {
  ## Then, we compute two subgraphs (for dde) in the case where there
  ## are output variables.  We'd actually only want to get the output
  ## variables that are time dependent I think, but that really should
  ## be all of them.
  if (obj$info$has_output) {
    is_output <-
    ## OK, what I need to find out here is:
    ##
    ##   * what is the full set of dependencies, including variables,
    ##     that are used in computing the output variables.
    ##
    ##   * what is *only* used in computing output variables
    ##
    ## This may change later...
    nms_output <- names(which(obj$traits[, "is_output"]))
    nms_deriv <- names(which(obj$traits[, "is_deriv"]))

    used_output <-
      setdiff(unique(c(unlist(obj$deps_rec[nms_output], use.names=FALSE),
                       nms_output)),
              TIME)
    used_output <- c(setdiff(used_output, names(obj$eqs)),
                     intersect(names(obj$eqs), used_output))
    used_deriv <- unique(c(unlist(obj$deps_rec[nms_deriv], use.names=FALSE),
                           nms_deriv))

    if (obj$info$has_delay) {
      ## TODO: need to do a little more work here; we don't have
      ## access to delay_support$order in general, but we can get at
      ## the dependencies that went into this, which we *do* have.
      stop("FIXME")
      ## browser()
      used_delay <-
        unique(unlist(lapply(obj$eqs[obj$traits[, "uses_delay"]],
                             function(x) x$rhs$delay_support$order)))
    } else {
      used_delay <- character(0)
    }

    only_output <- intersect(setdiff(used_output, c(used_deriv, used_delay)),
                             names_if(obj$stage == STAGE_TIME))

    ## This is the write bit:
    obj$output_info <-
      list(used=used_output, only=only_output, deriv=used_deriv)
    obj$stage[only_output] <- STAGE_OUTPUT
    for (i in intersect(only_output, names(obj$eqs))) {
      obj$eqs[[i]]$stage <- STAGE_OUTPUT
    }
  }
  obj
}

odin_parse_user <- function(obj) {
  ## We need to filter the dimensions off here for user arrays:
  is_user <- obj$traits[, "uses_user"] & !obj$traits[, "is_dim"]
  obj$user_default <-
    setNames(!vlapply(obj$eqs[is_user], function(x) x$rhs$default),
             obj$names_target[is_user])
  obj
}

odin_parse_check_unused <- function(obj) {
  ## Check for unused branches:
  v <- c("is_deriv", "is_output", "is_initial")
  endpoints <- names_if(apply(obj$traits[, v], 1, any))
  used <- union(endpoints,
                unique(unlist(obj$deps_rec[endpoints], use.names=FALSE)))
  unused <- !(names(obj$eqs) %in% used)
  if (any(unused)) {
    ## TODO: this is disabled for now because it breaks a ton of tests
    ## that I want to throw earlier.
    ##
    ## TODO: perhaps this should be a warning?
    ## odin_error(sprintf("Unused variables: %s",
    ##                    paste(names(obj$eqs)[unused], collapse=", ")),
    ##            get_lines(obj$eqs[unused]), get_exprs(obj$eqs[unused]))
  }
}
