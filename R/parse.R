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
  ret
}

odin_parse_expr <- function(i, exprs) {
  line <- utils::getSrcLocation(exprs[i], "line")
  x <- exprs[[i]]
  lhs <- odin_parse_lhs(x[[2L]], line, x)
  rhs <- odin_parse_rhs(x[[3L]], line, x)
  deps <- join_deps(list(lhs$depends, rhs$depends))

  list(name=lhs$name,
       lhs=lhs,
       rhs=rhs,
       depends=deps,
       expr=x,
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
      deps <- join_deps(lapply(index, find_symbols))
      valid <- c("-", "+", ":") # perhaps * and / are ok too?
      err <- setdiff(deps$functions, valid)
      if (length(err) > 0L) {
        odin_error(paste0("Invalid functions in array calculation: ",
                          paste(unique(err, collapse=", "))), line, expr)
      }

      nd <- length(index)
      if (nd > 3L) {
        odin_error(
          sprintf("Arrays must have at at most 3 dimensions (given %d)", nd),
          line, as.expression(expr))
      }

      if ("" %in% deps$variables) {
        odin_error("The empty index is not currently supported",
                   line, as.expression(expr))
      }

      ## TODO: Require all array arguments; don't allow for [] or [,1]
      ## perhaps?  Really depends if we can infer the length of an
      ## array.
      ##
      ## TODO: valid rhs functions are probably only [-, +, /, *, :]
      ret <- list(type="array",
                  name=deparse(lhs[[2L]]),
                  index=index,
                  nd=nd,
                  depends=deps)
    } else if (fun %in% SPECIAL) {
      if (length(lhs) != 2L) {
        odin_error("Invalid length special function on lhs", line, expr)
      }
      ret <- odin_parse_lhs(lhs[[2L]], line, expr)
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
    re <- sprintf("^(%s)_.*", paste(SPECIAL, collapse="|"))
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

    if ("user" %in% deps$functions) {
      if (length(rhs) > 2L) {
        odin_error("user() call must have zero or one argument", line, expr)
      }
      if (length(deps$variables) > 0L) {
        odin_error("user() call must not reference variables", line, expr)
      }
      deps$functions <- setdiff(deps$functions, "user")
      default <- length(rhs) == 2L
      list(type="expression",
           user=TRUE,
           default=FALSE,
           depends=deps,
           value=if (default) rhs[[2L]] else NULL)
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
  is_deriv <- vlapply(obj, function(x) identical(x$lhs$special, "deriv"))
  is_initial <- vlapply(obj, function(x) identical(x$lhs$special, "initial"))
  is_array <- vcapply(obj, function(x) x$lhs$type) == "array"

  nms <- vcapply(obj, "[[", "name")

  nms_real <- nms
  nms_real[is_deriv | is_initial] <-
    vcapply(obj[is_deriv | is_initial], function(x) x$lhs$name_target)

  err <- intersect(nms_real[is_array], nms_real[!is_array])
  if (length(err) > 0L) {
    i <- nms_real %in% err
    line <- viapply(obj[i], "[[", "line")
    expr <- as.expression(lapply(obj[i], "[[", "expr"))
    odin_error(sprintf("Array variables must always assign as arrays (%s)",
                       paste(err, collapse=", ")),
               line, expr)
  }

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
    x$expr <- lapply(obj[j], "[[", "expr")
    x$line <- viapply(obj[j], "[[", "line")

    nd <- viapply(obj[j], function(x) x[["lhs"]][["nd"]])
    if (length(unique(nd)) > 1L) {
      odin_error(sprintf("Array dimensionality is not consistent (%s)",
                         paste(sort(unique(nd)), collapse=", ")),
                 x$line, as.expression(x$expr))
    }

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

  ## Check that the derivatives, initial conditions agree in dimensionality
  check <- unique(nms_real[is_array & (is_deriv | is_initial)])

  drop <- unlist(lapply(i, "[", -1L))
  if (length(drop) > 0L) {
    obj <- obj[-drop]
    nms_real <- nms_real[-drop]
  }

  if (length(check) > 0L) {
    idx <- which(nms_real %in% check)
    for (i in split(idx, nms_real[idx])) {
      if (length(unique(lapply(obj[i], function(x) x$lhs$nd))) != 1L) {
        odin_error(
          "Array dimensionality is not consistent across initial/derivs",
          get_lines(obj[i]), get_exprs(obj[i]))
      }
    }
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
  exclude <- c("", "i", "j", "k", TIME)
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
  ## Determine if we are a user stage:
  is_user <- vlapply(obj[match(order, nms)], function(x) isTRUE(x$rhs$user))
  stage[is_user] <- STAGE_USER
  ## In topological order, determine inherited stage (a user/time stage
  ## anywhere in a chain implies a user/time stage).
  for (i in seq_along(order)) {
    stage[[i]] <- max(stage[[i]], stage[deps_rec[[i]]])
  }

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

  ## Should cycle through these in order I think; that way length can
  ## be inherited.  That requires doing a bit of pretty icky
  ## book-keeping.  Some of the array dimensions will be constants but
  ## depend on variables; but we can't compute those until we deal
  ## with the arrays!  So that's going to require this is integrated
  ## into an overall resolution approach.
  ##
  ## TODO: Compute length(arr) as a special thing and allow it to be
  ## used directly in code.  Similarly nrow(arr), ncol(arr) and
  ## dim(arr, n)
  ##
  ## NOTE: This is done iteratively, in dependency order, as some
  ## parts of this might use others?  Or do we want to replace with an
  ## lapply?
  ##
  ## TODO: This does not need to be done in this order now.
  for (i in match(intersect(order, nms[is_array]), nms)) {
    x <- obj[[i]]
    x$stage <- max(c(STAGE_CONSTANT, stage[x$lhs$depends$variables]))
    if (x$stage == STAGE_TIME) {
      odin_error("Array extent is determined by time",
                 x$line, as.expression(x$expr))
    }
    obj[[i]] <- x
  }

  nd <- setNames(viapply(obj[is_array], function(x) x$lhs$nd), nms[is_array])
  nd <- c(nd, setNames(nd[sprintf("deriv_%s", vars)], vars))

  uses_array <- vlapply(obj, function(x)
    any(x$depends$variables %in% nms[is_array]) ||
    any(x$depends$functions %in% "["))

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
       stage=stage_keep)
}

## I feel like I'm going to end up with a lot of these floating
## around; perhaps there's some nicer way of doing it...
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
        nok <- setdiff(sym$functions, c("-", "+", ":"))
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
STAGE_USER <- 2L
STAGE_TIME <- 3L
STAGES <- c("constant", "user", "time")
TIME <- "t"
## TODO: None of these deal with the use of these as functions (only
## variables) but that needs checking too.  Not 100% sure this is done
## on the lhs index bits.  Probably need to standardise that at some
## point.
SPECIAL <- c("initial", "deriv", "output")
RESERVED <- c("i", "j", "k", TIME, "user", SPECIAL)
