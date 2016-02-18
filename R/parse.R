## Read in the file and do the basic classification of all expressions.
##
## Coming out of parse we want to know:
##   * classification of all expressions into deriv, initial, var, output
##   * the set, and order, of ODE variables
##   * check that all ODE variables are initialised
##   * collect all array expressions
##   * do a topological sort
##
## Note that this is one big horrific linear function until I work out
## where the constituent parts are.  There will be some but they're
## not really obvious at the moment.
odin_parse <- function(file="", text=NULL) {
  exprs <- parse(file=file, text=text, keep.source=TRUE)

  ## First pass is to check that all operations are assignments.  No
  ## for loops, no if/else statements.  This might be relaxed later to
  ## allow if/else blocks to be created, but that's going to be
  ## potentially difficult because we'll need to check a bunch of
  ## branches.
  is_assignment <- function(x) {
    length(x) == 3L &&
      (identical(x[[1]], quote(`<-`)) || identical(x[[1]], quote(`=`)))
  }
  err <- which(!vlapply(exprs, is_assignment))
  if (length(err) > 0L) {
    odin_error("Every line must contain an assignment", err, exprs)
  }

  ## TODO: This will eventually run with some sort of error collection
  ## step so that all the errors are reported at once within this
  ## block.  Similar approaches will apply elsewhere.
  ret <- lapply(seq_along(exprs), odin_parse_expr, exprs)

  vars <- odin_parse_find_vars(ret)

  ## Rewrite initial conditions to depend not on the variables, but on
  ## initial conditions.
  ret <- odin_parse_rewrite_initial_conditions(ret, vars)
  ## Next, combine all array expressions together to a single
  ## expression.
  ret <- odin_parse_combine_arrays(ret)
  ret <- odin_parse_dependencies(ret, vars)
  ret <- odin_parse_variable_order(ret)
  ret
}

odin_parse_expr <- function(i, exprs) {
  line <- utils::getSrcLocation(exprs[i], "line")
  expr <- exprs[[i]]
  lhs <- odin_parse_lhs(expr[[2L]], line, expr)
  rhs <- odin_parse_rhs(expr[[3L]], line, expr)
  deps <- join_deps(list(lhs$depends, rhs$depends))

  if (isTRUE(rhs$user) && !is.null(lhs$special)) {
    odin_error("user() only valid for non-special variables", line, expr)
  }

  list(name=lhs$name,
       lhs=lhs,
       rhs=rhs,
       depends=deps,
       expr=expr,
       line=line)
}

odin_parse_lhs <- function(lhs, line, expr) {
  if (is.name(lhs)) {
    ret <- list(type="symbol",
         name=deparse(lhs))
  } else if (is.call(lhs)) {
    fun <- deparse_str(lhs[[1L]])
    if (fun %in% "[") { # NOTE: single indexing *only*
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

      if (any(vlapply(lhs[-1], identical, quote(expr=)))) {
        odin_error("The empty index is not currently supported",
                   line, as.expression(expr))
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
      tmp <- lapply(index, check_array_lhs_index)
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

      ret <- list(type="array",
                  name=deparse(lhs[[2L]]),
                  index=idx,
                  nd=nd,
                  depends=deps)
    } else if (fun %in% SPECIAL) {
      if (length(lhs) != 2L) {
        odin_error("Invalid length special function on lhs", line, expr)
      }
      ret <- odin_parse_lhs(lhs[[2L]], line, expr)
      if (fun == "dim") {
        if (ret$type != "symbol") {
          odin_error("dim() must be applied to a name only (not an array)",
                     line, expr)
        }
      }
      if (is.null(ret$special)) {
        ret$special <- fun
      } else {
        ## TODO: This should be called on the *final* iteration; this will
        ## give the wrong error message on
        ##   initial(initial(initial(x))) <- ...
        ## but noone should do that!
        odin_error("lhs functions require exactly one argument", line, expr)
      }
    } else {
      odin_error(sprintf("Unhandled expression %s on lhs", fun), line, expr)
    }
  } else { # things like atomic will raise here: 1 <- 2
    odin_error("Invalid left hand side", line, expr)
  }

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

odin_parse_rhs <- function(rhs, line, expr) {
  if (is.atomic(rhs)) {
    ## These are easy; they're constants so we can deal with these directly.
    ##
    ## Should check there that everything is of the classes: integer,
    ## logical, numeric only.  It's possible that strings would be
    ## possible but I'm not sure that's sensible.
    list(type="atomic", value=rhs)
  } else if (is.call(rhs) || is.name(rhs)) {
    deps <- find_symbols(rhs)
    err <- intersect(SPECIAL, deps$functions)
    if (length(err) > 0L) {
      odin_error(sprintf("Function %s is disallowed on rhs",
                         paste(unique(err), collapse=", ")), line, expr)
    }

    ## TODO: This is not acually the right move; if user is not the
    ## first call, then it should be an error too, so check for
    ## expr[[1]] being user and nothing else being user?
    if ("user" %in% deps$functions) {
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
      list(type="expression",
           depends=deps,
           value=if (default) rhs[[2L]] else NULL,
           default=default,
           user=TRUE)
    } else {
      list(type="expression",
           depends=deps,
           value=rhs)
    }
  } else {
    odin_error("Unhandled expression on rhs", line, expr)
  }
}

## So the pair:
##   x[1] <- ...
##   x[2:n] <- ...
## will get grouped together as a single x.  All dependencies of the
## expression will be combined, and the source reference also gets
## updated.
odin_parse_combine_arrays <- function(obj) {
  is_dim <- vlapply(obj, function(x) identical(x$lhs$special, "dim"))
  is_deriv <- vlapply(obj, function(x) identical(x$lhs$special, "deriv"))
  is_initial <- vlapply(obj, function(x) identical(x$lhs$special, "initial"))
  is_array <- vcapply(obj, function(x) x$lhs$type) == "array"

  nms <- vcapply(obj, "[[", "name")

  nms_real <- nms
  i <- is_deriv | is_initial | is_dim
  nms_real[i] <- vcapply(obj[i], function(x) x$lhs$name_target)
  names(nms_real) <- nms

  err <- intersect(nms_real[is_array & !is_dim],
                   nms_real[!is_array & !is_dim])
  if (length(err) > 0L) {
    i <- nms_real %in% err
    line <- viapply(obj[i], "[[", "line")
    expr <- as.expression(lapply(obj[i], "[[", "expr"))
    odin_error(sprintf("Array variables must always assign as arrays (%s)",
                       paste(err, collapse=", ")),
               line, expr)
  }

  ## TODO: I don't know if multiple dim() calls would be caught here
  ## (but they should be!)
  dup <- unique(nms[duplicated(nms)])
  err <- unique(nms[nms %in% dup & !is_array])
  if (length(err) > 0L) {
    i <- nms %in% err
    line <- viapply(obj[i], "[[", "line")
    expr <- as.expression(lapply(obj[i], "[[", "expr"))
    odin_error(sprintf("Duplicate entries must all be arrays (%s)",
                       paste(err, collapse=", ")),
               line, expr)
  }

  ## Now, work through the dim() calls so we establish dimensionality
  ## of arrays.
  nd <- setNames(viapply(obj[is_dim], check_dim_rhs), nms_real[is_dim])
  if (any(is.na(nd))) {
    i <- which(is_dim)[is.na(nd)]
    odin_error("Invalid dim() rhs", get_lines(i), get_exprs(i))
  }
  j <- which(is_dim)
  for (i in seq_along(nd)) {
    obj[[j[[i]]]]$nd <- nd[[i]]
  }

  ## Then, work out which sets to combine
  i <- match(nms, unique(nms[is_array]))
  i <- unname(split(which(!is.na(i)), na.omit(i)))

  for (j in i) {
    k <- j[[1]]
    x <- obj[[k]]
    x$depends <-
      join_deps(lapply(obj[j], function(x) x[["depends"]]))
    ## NOTE: this is the only case where self referential variables
    ## are allowed.  There's no checking here and things like
    ##   x[i] = x[i] * 2
    ## will cause a crash or nonsense behaviour.
    x$depends$variables <- setdiff(x$depends$variables, x$name)
    if (!is_dim[k]) {
      x$depends$variables <- union(x$depends$variables,
                                   sprintf("dim_%s", nms_real[[x$name]]))
    }
    x$expr <- lapply(obj[j], "[[", "expr")
    x$line <- viapply(obj[j], "[[", "line")

    nd_x <- tryCatch(nd[[nms_real[[x$name]]]],
                     error=function(e)
                       odin_error(
                         sprintf("No dim() call found for %s", x$name),
                         x$line, as.expression(x$expr)))

    err <- viapply(obj[j], function(x) x[["lhs"]][["nd"]]) != nd_x
    if (any(err)) {
      odin_error(
        sprintf("Array dimensionality is not consistent (expected %d %s)",
                nd_x, ngettext(nd_x, "index", "indices")),
        get_lines(obj[err]), get_exprs(obj[err]))
    }

    ## TODO: some of the lhs depends stuff will not matter so much now.
    x$lhs$index <- lapply(obj[j], function(x) x[["lhs"]][["index"]])
    x$lhs$depends <-
      join_deps(lapply(obj[j], function(x) x[["lhs"]][["depends"]]))

    x$rhs$type <- vcapply(obj[j], function(x) x[["rhs"]][["type"]])
    x$rhs$depends <-
      join_deps(lapply(obj[j], function(x) x[["rhs"]][["depends"]]))
    x$rhs$value <- lapply(obj[j], function(x) x[["rhs"]][["value"]])

    ## Sanity check:
    ok <- c("name", "lhs", "rhs", "depends", "expr", "line")
    stopifnot(length(setdiff(unlist(lapply(obj[j], names)), ok)) == 0L)
    ## NOTE: mixed type specials are dealt with elsewhere.  By this I
    ## mean that a variable is more than one of initial(), deriv(),
    ## output() and plain.
    ok <- c("type", "name", "name_target", "index", "nd", "depends", "special")
    stopifnot(length(setdiff(unlist(lapply(
      obj[j], function(x) names(x$lhs))), ok)) == 0L)
    ok <- c("type", "depends", "value")
    stopifnot(length(setdiff(unlist(lapply(
      obj[j], function(x) names(x$rhs))), ok)) == 0L)

    obj[[k]] <- x
  }

  drop <- unlist(lapply(i, "[", -1L))
  if (length(drop) > 0L) {
    obj <- obj[-drop]
    nms_real <- nms_real[-drop]
  }

  obj
}

odin_parse_find_vars <- function(obj) {
  ## Next, identify initial conditions and derivatives.
  is_deriv <- vlapply(obj, function(x) identical(x$lhs$special, "deriv"))
  is_initial <- vlapply(obj, function(x) identical(x$lhs$special, "initial"))

  nms <- vcapply(obj, "[[", "name")

  name_target <- function(x) x$lhs$name_target
  vars <- unique(vcapply(obj[is_deriv], name_target))
  vars_initial <- unique(vcapply(obj[is_initial], name_target))

  ## And from these, determine the names of the core variables.  The
  ## order will be the same as the order they are first defined in the
  ## file (so for an array calculation this is important).
  if (!setequal(vars, vars_initial)) {
    ## TODO: Getting a nicer error message here with the missing and
    ## extra equations would be nice, but can wait.
    stop("derivs() and initial() must contain same set of equations")
  }

  err <- nms %in% vars
  if (any(err)) {
    line <- viapply(obj[err], "[[", "line")
    expr <- as.expression(lapply(obj[err], "[[", "expr"))
    ## TODO: _or_ the rhs of anything.  Could be clearer.
    odin_error(
      sprintf("variables on lhs must be within deriv() or initial() (%s)",
              paste(intersect(vars, nms), collapse=", ")), line, expr)
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
odin_parse_rewrite_initial_conditions <- function(obj, vars) {
  i <- vlapply(obj, function(x) (identical(x$lhs$special, "initial") &&
                                 any(vars %in% x$rhs$depends$variables)))
  if (any(i)) {
    replace <- function(x, tr) {
      i <- match(x, names(tr))
      j <- !is.na(i)
      x[j] <- tr[i][j]
      x
    }
    rewrite_expression <- function(expr, env) {
      eval(substitute(substitute(y, env), list(y = expr)))
    }
    warning("This is untested", immediate.=TRUE)
    subs <- setNames(sprintf("initial_%s", vars), vars)
    env <- as.environment(lapply(subs, as.name))
    f <- function(x) {
      if (any(vars %in% x$rhs$depends$variables)) {
        x$rhs$value <- rewrite_expression(x$rhs$value, env)
        x$rhs$depends$variables <- replace(x$rhs$depends$variables, subs)
        x$depends$variables <- replace(x$depends$variables, subs)
      }
      x
    }
    obj[i] <- lapply(obj[i], f)
  }
  obj
}

odin_parse_dependencies <- function(obj, vars) {
  nms <- vcapply(obj, "[[", "name")
  exclude <- c("", INDEX, TIME)
  deps <- lapply(obj, function(el) setdiff(el$depends$variables, exclude))
  names(deps) <- nms

  msg <- lapply(deps, setdiff, c(nms, vars))
  i <- lengths(msg) > 0L
  if (any(i)) {
    odin_error(sprintf("Unknown variables %s",
                       paste(sort(unique(unlist(msg))), collapse=", ")),
               get_lines(obj[i]), get_exprs(obj[i]))
  }

  ## For the derivative calculations the variables come in with no
  ## dependencies because they are provided by the integrator, but
  ## we'll add an implicit time dependency.
  dummy <- c(list(t=character(0)),
             setNames(rep(list(TIME), length(vars)), vars))
  order <- topological_order(c(deps, dummy))

  ## Then, we work out the recursive dependencies; this is the entire
  ## dependency chain of a thing; including its dependencies, its
  ## dependencies dependencies and so on.
  deps_rec <- recursive_dependencies(order, c(deps, dummy), vars)

  ## Then, we can get the stage for these variables:
  stage <- setNames(rep(STAGE_CONSTANT, length(order)), order)
  stage[TIME] <- STAGE_TIME
  ## In topological order, determine inherited stage (a initial/time stage
  ## anywhere in a chain implies a initial/time stage).
  for (i in seq_along(order)) {
    stage[[i]] <- max(stage[[i]], stage[deps_rec[[i]]])
  }

  ## Adjust the order so that it's by stage first, and then the order.
  ## This should not create any impossible situations because of the
  ## stage treatent above.
  i <- order(stage)
  stage <- stage[i]
  order <- order[i]

  ## This is the point where we have to give up and start creating a
  ## real object; it probably should have been the step previous
  ## (TODO).
  order_keep <- setdiff(order, names(dummy))
  stage_keep <- stage[nms]
  for (i in seq_along(nms)) {
    obj[[i]]$stage <- stage_keep[[i]]
  }
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
  is_array <- vlapply(obj, function(x) identical(x$lhs$type, "array"))
  is_dim <- vlapply(obj, function(x) identical(x$lhs$special, "dim"))

  ## TODO: Compute length(arr) as a special thing and allow it to be
  ## used directly in code.  Similarly allow dim(arr, {1,2,3}), stored
  ## internaly as dim_1, dim_2, dim_3; can do that with code rewriting
  ## easily enough; make dim an allowed function in the rhs arrays and
  ## lhs if not a cyclic dependency (this is all done apart from
  ## _using_ dim).
  dim_stage <- viapply(obj[is_dim], function(x) x$stage)
  i <- dim_stage == STAGE_TIME
  if (any(i)) {
    odin_error("Array extent is determined by time",
               get_lines(obj[i]), get_exprs(obj[i]))
  }

  ## Determine which variables are array extents and indices; we'll
  ## flag these as integers.  At the same time we need to try and work
  ## out which of these are confusing (perhaps used as an argument to
  ## division).
  index_vars <- c(lapply(obj[is_dim], function(x) x$rhs$depends$variables),
                  lapply(obj[is_array], function(x) x$lhs$depends$variables))
  all_index_vars <- unique(unlist(index_vars))
  err <- intersect(index_vars, nms[is_array])
  if (length(err) > 0L) {
    i <- which(is_array)[vlapply(obj[is_array], function(x)
      any(err %in% x$lhs$depends$variables))]
    odin_error(sprintf("Array indices may not be arrays (%s used)",
                       pastec(err)),
               get_lines(obj[i]), get_exprs(obj[i]))
  }

  if (TIME %in% all_index_vars) {
    i <- which(is_array)[vlapply(obj[is_array], function(x)
      any(TIME %in% x$lhs$depends$variables))]
    odin_error("Array indices may not be time",
               get_lines(obj[i]), get_exprs(obj[i]))
  }

  ## How do we determine that these are not used as floats anywhere?
  ## The actual variables are already filtered out.
  uses_array <- vlapply(obj, function(x)
    any(x$depends$variables %in% nms[is_array]) ||
    any(x$depends$functions %in% "["))
  ## Number of dimensions for each variable array variable.
  nd <- setNames(viapply(obj[is_array], function(x) x$lhs$nd), nms[is_array])
  nd <- c(nd, setNames(nd[sprintf("deriv_%s", vars)], vars))
  for (i in which(uses_array)) {
    ok <- check_array_rhs(obj[[i]]$rhs$value, nd)
    if (!ok) {
      msg <- paste0("\t\t", attr(ok, "message"), collapse="\n")
      odin_error(sprintf("Invalid array use on rhs:\n%s", msg),
                 obj[[i]]$line, as.expression(obj[[i]]$expr))
    }
    obj[[i]]$rhs$uses_array <- TRUE
  }

  list(vars=vars,
       eqs=obj,
       order=order_keep,
       stage=stage_keep,
       index_vars=all_index_vars)
}

odin_parse_variable_order <- function(obj) {
  ## Within these classes, is it best to put them in topological order
  ## or in lexical order?  Start with lexical order as we can always
  ## tweak that later.
  ##
  ## I think it should be sorted by stage within the arrays though,
  ## and stage added here.
  is_deriv <- vlapply(obj$eqs, function(x) identical(x$lhs$special, "deriv"))
  tmp <- obj$eqs[is_deriv]
  is_array <- vlapply(tmp, function(x) x$lhs$type == "array")
  nms <- vcapply(tmp, function(x) x$lhs$name_target)

  stage <- rep(STAGE_CONSTANT, length(is_array))
  stage[is_array] <- unname(obj$stage[sprintf("dim_%s", nms[is_array])])

  i <- order(is_array + stage)
  tmp <- tmp[i]
  nms <- nms[i]
  stage <- setNames(stage[i], nms)
  is_array <- setNames(is_array[i], nms)

  offset <- setNames(as.list(seq_along(is_array) - 1L), nms)
  f <- function(i) {
    if (i == 1L) {
      0L
    } else if (!is_array[[i - 1L]]) {
      offset[[i - 1L]] + 1L
    } else if (identical(offset[[i - 1L]], 0L)) {
      as.name(paste0("dim_", nms[[i - 1L]]))
    } else {
      call("+",
           as.name(paste0("offset_", nms[[i - 1L]])),
           as.name(paste0("dim_", nms[[i - 1L]])))
    }
  }
  for (i in which(is_array)) {
    offset[[i]] <- f(i)
  }
  offset_is_var <- !vlapply(offset, is.numeric)
  offset_use <- offset
  offset_use[offset_is_var] <- sprintf("offset_%s", nms[offset_is_var])

  total <- f(length(is_array) + 1L)
  total_is_var <- !is.numeric(total)
  total_use <- if (total_is_var) "dim" else total
  total_stage <- max(stage)

  obj$variable_order <-
    list(order=nms, is_array=is_array,
         offset=offset, offset_use=offset_use,
         offset_is_var=offset_is_var,
         total=total,
         total_is_var=total_is_var,
         total_use=total_use,
         total_stage=total_stage)
  obj
}

## I feel like I'm going to end up with a lot of these floating
## around; perhaps there's some nicer way of doing it...
##
## TODO: Need to pass through here information about the rangedness of
## the lhs and filter i, j, k by that.
check_array_rhs <- function(expr, nd) {
  if (is.list(expr)) {
    res <- lapply(expr, check_array_rhs, nd)
    msg <- as.character(unlist(lapply(res, attr, "message", exact=TRUE)))
    return(structure(all(vlapply(res, as.logical)), message=msg))
  }

  nms <- names(nd)
  err <- collector()

  leaf <- function(e, w) {
    if (!is.symbol(e)) { # A literal of some type
      return()
    } else if (deparse(e) %in% nms) {
      err$add(sprintf("Found %s on rhs", deparse(e)))
    }
  }
  ## Descend down the call tree until reaching a `[` call, then
  ## analyse that call with a more restricted set of rules.
  call <- function (e, w) {
    if (identical(e[[1L]], quote(`[`))) {
      x <- deparse(e[[2L]])
      ijk <- as.list(e[-(1:2)])
      if (x %in% nms) {
        if (length(ijk) != nd[[x]]) {
          err$add("Incorrect dimensionality for %s in '%s'", x, deparse_str(e))
        }
        sym <- find_symbols(ijk)
        nok <- setdiff(sym$functions, VALID_ARRAY)
        if (length(nok) > 0L) {
          err$add("Disallowed functions used for %s in '%s': %s",
                  x, deparse_str(e), pastec(nok))
        }
        nok <- intersect(sym$variables,  c("", nms))
        if (length(nok) > 0L) {
          err$add("Disallowed variables used for %s in '%s': %s",
                  x, deparse_str(e), pastec(nok))
        }
      } else {
        err$add("Unknown array variable %s in '%s'", x, deparse_str(e))
      }
    } else {
      for (a in as.list(e[-1])) {
        if (!missing(a)) {
          codetools::walkCode(a, w)
        }
      }
    }
  }

  walker <- codetools::makeCodeWalker(call=call, leaf=leaf, write=cat)
  codetools::walkCode(expr, walker)
  x <- unique(err$get())
  ok <- length(x) == 0L
  if (ok) TRUE else structure(FALSE, message=x)
}

check_dim_rhs <- function(x) {
  if (x$rhs$type == "atomic" || is.name(x$rhs$value)) {
    1L
  } else if (identical(x$rhs[[1]], quote(c))) {
    browser()
    ok <- vlapply(as.list(x$rhs[-1L]),
                  function(x) is.symbol(x) || is.atomic(x))
    if (!all(ok)) {
      NA_integer_
    }
    length(x$rhs) - 1L
  } else {
    NA_integer_
  }
}

check_array_lhs_index <- function(x) {
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
        }
        if (!(nm %in% valid)) {
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

odin_error <- function(msg, line, expr) {
  if (is.expression(expr)) {
    expr_str <- vcapply(expr, deparse_str)
  } else {
    expr_str <- deparse_str(expr)
  }
  str <- sprintf("%s # (line %s)", expr_str, line)
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

STAGE_CONSTANT <- 1L
STAGE_INITIAL <- 2L
STAGE_TIME <- 3L
STAGES <- c("constant", "user", "time")
TIME <- "t"
STATE <- "state"
DSTATEDT <- "dstatedt"
## TODO: None of these deal with the use of these as functions (only
## variables) but that needs checking too.  Not 100% sure this is done
## on the lhs index bits.  Probably need to standardise that at some
## point.
SPECIAL <- c("initial", "deriv", "output", "dim")
INDEX <- c("i", "j", "k")
RESERVED <- c(INDEX, TIME, STATE, DSTATEDT, "user", SPECIAL)
RESERVED_PREFIX <- c(SPECIAL, "dim", "odin", "offset")
VALID_ARRAY <- c("-", "+", ":", "(")
