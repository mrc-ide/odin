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
##    order in which to traverse the model.  This step also determines
##    if expressions are constant or time dependent ("stage").
##
## 9. Compute the order to store variables in, and associated bits for
##    extraction.  These are used in odin_parse_delay() too.
##
## 10. Collect some information about initial conditions, user
##     variables, outputs, delays and interpolate calls.

## Read in the file and do the basic classification of all expressions.
odin_parse <- function(x) {
  ## 1. Basic preparations over the expression list:
  exprs <- odin_parse_prepare(x)

  ## 2. Prepare each expression:
  eqs <- odin_parse_exprs(exprs)

  ## Start building the core object:
  if (is.character(x) && length(x) == 1L && file.exists(x)) {
    file <- x
    path <- c(normalizePath(dirname(x)), normalizePath(getwd()))
  } else {
    file <- basename(tempfile("odin", "."))
    path <- getwd()
  }
  ret <- list(eqs=eqs, file = file, path = path)

  ## 3. Compute overall information on traits (creates elements $traits
  ## and $info):
  ret <- odin_parse_collect_traits(ret)

  ## 4. Identify all ODE variables:
  ret$vars <- odin_parse_find_vars(ret$eqs, ret$traits, ret$info)

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
  ret <- odin_parse_arrays(ret)

  ## 8. Determine the graph structure of the model and (from that) the
  ## order in which to traverse the model.
  ret <- odin_parse_dependencies(ret)

  ## 9. Compute the order to store variables in, and associated bits
  ## for extraction.  These are used in odin_parse_delay() too.
  ret <- odin_parse_variable_info(ret)

  ## 10. Collect some information about initial conditions, user
  ## variables, outputs, delays and interpolate calls.
  ret <- odin_parse_usage(ret)
  ret <- odin_parse_initial(ret)
  ret <- odin_parse_user(ret)
  ret <- odin_parse_output(ret)
  ret <- odin_parse_delay(ret)
  ret <- odin_parse_interpolate(ret)
  ret <- odin_parse_check_functions(ret)

  ## 11. Report any unused variables
  odin_parse_check_unused(ret)

  ret
}

odin_parse_prepare <- function(x) {
  parse_expression <- function(x) {
    if (inherits(x, "{")) {
      as.expression(as.list(x[-1L]))
    } else {
      as.expression(x)
    }
  }
  expr_is_assignment <- function(x) {
    length(x) == 3L &&
      (identical(x[[1]], quote(`<-`)) || identical(x[[1]], quote(`=`)))
  }
  exprs <- switch(odin_parse_prepare_detect(x),
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

odin_parse_prepare_detect <- function(x) {
  if (is.language(x)) {
    as <- "expression"
  } else if (is.character(x)) {
    ## We're really looking for a separator given that we need
    if (length(x) > 1L || grepl("[\n;]", x)) {
      as <- "text"
    } else if (file.exists(x)) {
      as <- "file"
    } else {
      stop("'x' looks like a filename, but file does not exist")
    }
  } else {
    stop("Invalid type for 'x'")
  }
  as
}

odin_parse_collect_traits <- function(obj) {
  eqs <- obj$eqs
  nms <- vcapply(eqs, "[[", "name")

  ## special lhs:
  special <- vcapply(eqs, function(x) x$lhs$special %||% "")
  is_dim <- special == "dim"
  is_deriv <- special == "deriv"
  is_update <- special == "update"
  is_initial <- special == "initial"
  is_output <- special == "output"
  is_config <- special == "config"

  discrete <- any(is_update)
  if (discrete && any(is_deriv)) {
    tmp <- eqs[is_deriv | is_update]
    odin_error("Cannot mix deriv() and update()",
               get_lines(tmp), get_exprs(tmp))
  }
  if (discrete) {
    is_deriv <- is_update
  }

  ## core rhs:
  is_array <- vcapply(eqs, function(x) x$lhs$type) == "array"
  is_symbol <- vcapply(eqs, function(x) x$lhs$type) == "symbol"

  ## rhs behaviour
  uses_atomic <- vlapply(eqs, function(x) identical(x$rhs$type, "atomic"))
  uses_user <- vlapply(eqs, function(x) isTRUE(x$rhs$user))
  uses_delay <- vlapply(eqs, function(x) isTRUE(x$rhs$delay))
  uses_interpolate <- vlapply(eqs, function(x) isTRUE(x$rhs$interpolate))
  uses_sum <- vlapply(eqs, function(x) isTRUE(x$rhs$sum))
  uses_stochastic <- vlapply(eqs, "[[", "stochastic")

  traits <- cbind(is_dim, is_deriv, is_initial, is_output, is_config,
                  is_array, is_symbol,
                  uses_atomic, uses_user, uses_delay,
                  uses_interpolate, uses_sum, uses_stochastic)
  rownames(traits) <- names(eqs)

  obj$traits <- traits
  obj$info <- list(discrete=discrete,
                   ## internal really, and may change
                   target_name=if (discrete) "update" else "deriv",
                   target_name_fn=if (discrete) update_name else deriv_name,
                   ## external again
                   has_array=any(is_array) || any(is_dim),
                   has_output=any(is_output),
                   has_user=any(uses_user),
                   has_delay=any(uses_delay),
                   has_interpolate=any(uses_interpolate),
                   has_sum=any(uses_sum),
                   has_stochastic=any(uses_stochastic))

  nms_target <- names(eqs)
  i <- is_deriv | is_update | is_initial | is_dim | is_output
  nms_target[i] <- vcapply(eqs[i], function(x) x$lhs$name_target)
  obj$names_target <- nms_target

  obj
}

## Identfying variables is straightforward; they have deriv() and
## initial() calls.  It is an error not to have both.
odin_parse_find_vars <- function(eqs, traits, info) {
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
      msg$add("\tin %s() but not initial(): %s",
              info$target_name, paste(msg_initial, collapse=", "))
    }
    msg_vars <- setdiff(vars_initial, vars)
    if (length(msg_vars) > 0L) {
      msg$add("\tin initial() but not %s(): %s",
              info$target_name, paste(msg_vars, collapse=", "))
    }
    tmp <- eqs[is_deriv | is_initial]
    odin_error(sprintf(
      "%s() and initial() must contain same set of equations:\n%s\n",
      info$target_name, paste(msg$get(), collapse="\n")),
      get_lines(tmp), get_exprs(tmp))
  }

  err <- names(is_deriv) %in% vars
  if (any(err)) {
    odin_error(
      sprintf("variables on lhs must be within %s() or initial() (%s)",
              info$target_name,
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
  subs <- setNames(initial_name(vars), vars)
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

odin_parse_variable_info <- function(obj) {
  obj$variable_info <- odin_parse_extract_order(obj)
  obj
}

## Given a vector of equation naems, let's unpack things:
odin_parse_extract_order <- function(obj, output = FALSE, subset = NULL) {
  ## TODO: we'll need to test this in a case where a subset of
  ## variables are pulled out.  Joel's will make a nice integration
  ## test though I believe.
  if (output) {
    names <- names_if(obj$traits[, "is_output"])
  } else {
    vars <- if (is.null(subset)) obj$vars else intersect(obj$vars, subset)
    names <- obj$info$target_name_fn(vars)
  }

  n <- length(names)
  is_array <- obj$traits[names, "is_array"]

  ord <- integer(n)
  array <- integer(n)
  stage_dim <- rep_len(STAGE_CONSTANT, n)

  if (any(is_array)) {
    tmp <- vcapply(obj$eqs[names][is_array], function(x) x$lhs$name_dim)
    ord[is_array] <- match(tmp, names(obj$eqs))
    ## TODO: change to function(x) x$lhs$nd ?
    array[is_array] <- viapply(obj$eqs[tmp], "[[", "nd")
    stage_dim[is_array] <- obj$stage[tmp]

    ## Reorder to get variables ordered to scalars first
    i <- order(ord)
    names <- names[i]
    is_array <- is_array[i]
    array <- array[i]
    stage_dim <- stage_dim[i]
  }

  names_target <- obj$names_target[match(names, names(obj$eqs))]
  names_offset <- offset_name(names_target, output)

  len <- rep_len(list(1L), n)
  len[is_array] <- vcapply(names_target[is_array], array_dim_name)
  len_is_var <- vlapply(len, is.language)

  tmp <- variable_offsets(names_target, is_array, len, output)
  offset <- tmp$offset
  total <- tmp$total

  offset_is_var <- vlapply(offset, is.language)
  total_is_var <- is.language(total)

  offset_use <- ifelse(offset_is_var, as.list(names(offset)), offset)
  if (total_is_var) {
    total_use <- if (output) "dim_output" else "dim"
  } else {
    total_use <- total
  }

  total_stage <- max(c(STAGE_CONSTANT, stage_dim))

  list(order=names_target,
       n=n,
       names=names,
       is_array=is_array, # drop in favour of array > 0?
       array=array,
       offset=offset,
       offset_use=offset_use,
       len=len,
       len_is_var=len_is_var,
       total=total,
       total_is_var=total_is_var,
       total_use=total_use,
       total_stage=total_stage)
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
odin_parse_interpolate <- function(obj) {
  if (!obj$info$has_interpolate) {
    return(obj)
  }

  check_interpolate_arg <- function(nm, rank) {
    target <- obj$eqs[[nm]]
    if (target$lhs$type != "array" || target$lhs$nd != rank) {
      type <- if (rank == 1L) "vector" else paste(rank, "dimensional array")
      odin_error(sprintf("Expected %s to be a %s", nm, type),
                 target$line, as.expression(target$expr))
    }
  }
  process1 <- function(e) {
    rank <- if (e$lhs$type == "symbol") 0L else e$lhs$nd
    ## NOTE: interpolating 3d arrays requires 4d input arrays.
    ## Implementing these is not done and will require a fair bit of
    ## work and tweakery (we don't have an automatic index array so
    ## that's not nice).  But *supplying* the required 4d arrays is
    ## likely to be horrid so it's probably not going to be needed for
    ## a while.
    ##
    ## NOTE: we check for single line assignment in odin_parse_arrays_1
    value <- if (rank == 0L) e$rhs$value else e$rhs$value[[1L]]
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
  odin_info(msg, line, expr, function(...) stop(..., call.=FALSE))
}
odin_note <- function(msg, line, expr) {
  odin_info(paste("Note: ", msg), line, expr, message)
}

odin_info <- function(msg, line, expr, announce) {
  if (is.expression(expr)) {
    expr_str <- vcapply(expr, deparse_str)
  } else {
    expr_str <- deparse_str(expr)
  }
  str <- sprintf(ifelse(is.na(line), "%s", "%s # (line %s)"), expr_str, line)
  announce(msg, paste0("\n\t", str, collapse=""))
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
  grepl("^[A-Za-z_][A-Za-z0-9_]*", x) & !(x %in% c_reserved)
}


######################################################################

odin_parse_initial <- function(obj) {
  nms_initial <- names_if(obj$traits[, "is_initial"])

  initial_stage <- (if (obj$info$has_delay) STAGE_TIME
                    else max(c(STAGE_CONSTANT, obj$stage[nms_initial])))

  obj$info$initial_stage <- initial_stage
  obj
}

odin_parse_user <- function(obj) {
  ## We need to filter the dimensions off here for user arrays:
  is_user <- obj$traits[, "uses_user"] & !obj$traits[, "is_dim"]
  obj$info$user_default <-
    setNames(!vlapply(obj$eqs[is_user], function(x) x$rhs$default),
             obj$names_target[is_user])
  obj
}

odin_parse_check_unused <- function(obj) {
  deps <- obj$deps_rec
  ## Check for unused branches:
  v <- c("is_deriv", "is_output", "is_initial")
  endpoints <- names_if(apply(obj$traits[, v], 1, any))
  used <- union(endpoints,
                unique(unlist(deps[endpoints], use.names=FALSE)))
  if (obj$info$has_delay) {
    delays <- intersect(names_if(obj$traits[, "uses_delay"]), used)
    delay_vars <- lapply(obj$eqs[delays], function(x)
      x$rhs$depends_delay$variables)
    used <- union(used, c(unlist(delay_vars),
                          unlist(deps[unlist(unique(delay_vars))])))
  }

  unused <- !(names(obj$eqs) %in% used)
  if (any(unused)) {
    ## NOTE: at this point it would be nicest to unravel the
    ## dependency graph a bit to find the variables that are really
    ## never used; these are the ones that that the others come from.
    ## But at this point all of these can be ripped out so we'll just
    ## report them all:
    ##
    ## TODO: We should have this be tuneable; ignore/message/warning/error
    nms <- unique(obj$names_target[unused])
    what <- ngettext(length(nms), "variable", "variables")
    tmp <- obj$eqs[sort(which(unused))]
    odin_note(sprintf("Unused %s: %s", what, pastec(nms)),
              get_lines(tmp), get_exprs(tmp))
  }
}

odin_parse_check_functions <- function(obj) {
  used_functions <- lapply(obj$eqs, function(x) x$depends$functions)
  all_used_functions <- unique(unlist(used_functions))

  if (!obj$info$discrete) {
    err <- intersect(all_used_functions, names(FUNCTIONS_STOCHASTIC))
    if (length(err) > 0L) {
      tmp <- obj$eqs[vlapply(used_functions, function(x) any(x %in% err))]
      odin_error(sprintf(
        "Stochastic functions not allowed in ODE models (used: %s)",
        pastec(err)),
        get_lines(tmp), get_exprs(tmp))
    }
  }

  allowed <- c(names(FUNCTIONS),
               names(FUNCTIONS_INFIX),
               names(FUNCTIONS_UNARY),
               names(FUNCTIONS_RENAME),
               if (obj$info$discrete) names(FUNCTIONS_STOCHASTIC),
               FUNCTIONS_SUM,
               names(obj$config$include$declarations))

  err <- setdiff(all_used_functions, allowed)
  if (length(err) > 0L) {
    tmp <- obj$eqs[vlapply(used_functions, function(x) any(x %in% err))]
    odin_error(sprintf("Unsupported %s: %s",
                       ngettext(length(err), "function", "functions"),
                       pastec(err)),
               get_lines(tmp), get_exprs(tmp))
  }

  obj
}

odin_parse_usage <- function(obj) {
  deps_rec <- obj$deps_rec

  nms_initial <- names_if(obj$traits[, "is_initial"])
  nms_deriv <- names_if(obj$traits[, "is_deriv"])
  nms_output <- names_if(obj$traits[, "is_output"])
  nms_delay <- names_if(obj$traits[, "uses_delay"])

  get_used <- function(x) {
    res <- unique(c(unlist(obj$deps_rec[x], use.names=FALSE), x))
    ## Re-order to put time and variables first
    c(setdiff(res, names(obj$eqs)), intersect(names(obj$eqs), res))
    ## Possibly we should also be subsetting by time dependence?
    ## intersect(res, names_if(obj$stage == STAGE_TIME))
  }

  used <- list(initial = get_used(nms_initial),
               deriv   = get_used(nms_deriv),
               output  = get_used(nms_output),
               delay   = get_used(nms_delay))
  if (obj$info$discrete) {
    names(used)[names(used) == "deriv"] <- "update"
  }

  ## NOTE: putting this into info so that it's available in generate2
  obj$info$eqs_used <- used

  obj
}

is_dim_or_length <- function(x) {
  is_call(x, quote(dim)) || is_call(x, quote(length))
}

variable_offsets <- function(names, is_array, len, output = FALSE) {
  n <- length(names)
  names_offset <- offset_name(names, output)
  offset <- setNames(as.list(seq_len(n) - 1L), names_offset)
  accumulate_offset <- function(i) {
    if (i == 1L) {
      0L
    } else if (!is_array[[i - 1L]]) {
      offset[[i - 1L]] + 1L
    } else if (identical(offset[[i - 1L]], 0L)) {
      as.name(array_dim_name(names[[i - 1L]]))
    } else {
      prev <- offset[[i - 1L]]
      if (is.language(prev)) {
        prev <- as.name(names_offset[[i - 1L]])
      }
      call("+", prev, as.name(len[[i - 1L]]))
    }
  }
  for (i in which(is_array)) {
    offset[[i]] <- accumulate_offset(i)
  }

  total <- accumulate_offset(n + 1L)

  list(offset = offset, total = total)
}
