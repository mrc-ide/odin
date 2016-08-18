## The general approach here is:
##
## 1. coerce whatever we're given into a list of assignments
##
## 2. parse each expression into core types, along with a small amount
##    of rewriting.  This stage does all the bits that can be done to
##    each expression without reference to any other expression.
##      * odin_parse_expr (in parse_expr.R)
##        -         _expr_lhs{,_index,_special}
##        -         _expr_rhs{,_delay,_user,_interpolate,_sum}
##
## 3. ...undocumented...

## Read in the file and do the basic classification of all expressions.
odin_parse <- function(x, as="file") {
  ## Basic preparations over the expression list:
  exprs <- odin_parse_prepare(x, as)

  ## Prepare each expression:
  eqs <- odin_parse_exprs(exprs)

  ## Start building the core object:
  ret <- list(eqs=eqs,
              file=if (as == "file") x else basename(tempfile("odin", ".")),
              vars=odin_parse_find_vars(eqs),
              traits=odin_parse_collect_traits(eqs))
  names(ret$eqs) <- rownames(ret$traits)

  ## Compute overall information on types:
  ret$info <- list(has_array=any(ret$traits[, "is_array"]),
                   has_output=any(ret$traits[, "is_output"]),
                   has_user=any(ret$traits[, "uses_user"]),
                   has_delay=any(ret$traits[, "uses_delay"]),
                   has_interpolate=any(ret$traits[, "uses_interpolate"]),
                   has_sum=any(ret$traits[, "uses_sum"]))

  ## Then below here everything is done via modifying the object
  ## directly.  Things may (and do) modify more than one element of
  ## the object here.  During the cleanup I'll try and work out what
  ## is going on though.

  ## The order here does matter, but it's not documented which depends
  ## on which yet.
  ret <- odin_parse_config(ret)
  ret <- odin_parse_process_interpolate(ret)
  ret <- odin_parse_rewrite_initial_conditions(ret)
  ret <- odin_parse_combine_arrays(ret)
  ret <- odin_parse_dependencies(ret)
  ret <- odin_parse_check_array_usage(ret)
  ret <- odin_parse_variable_order(ret)
  ret <- odin_parse_delay(ret)
  ret <- odin_parse_output(ret)
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


odin_parse_collect_traits <- function(eqs) {
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

  rownames(traits) <- nms
  traits
}

odin_parse_config <- function(obj) {
  ## We'll test for allowed config keys here:
  ##    base - basename
  ##    method - this is the default only
  ##    atol, rtol - defaults only
  ##    [we'll accept any defaults here?]

  ## If base is not given, then we'll look at the actual underlying
  ## filename; if that is not given, use odin?

  ## That will scale pretty well for packages where we'll want each
  ## different file to have their own base.
  cfg <- obj$eqs[rownames(obj$traits)[obj$traits[, "is_config"]]]

  ## Then, we go through and get the names of the variables.  They
  ## must all be symbols, rather than characters, but that's enforced
  ## earlier
  err <- !obj$traits[names(cfg), "uses_atomic"]
  if (any(err)) {
    odin_error("config() rhs must be atomic (not an expression)",
               get_lines(cfg[err]), get_exprs(cfg[err]))
  }

  dat <- setNames(lapply(cfg, function(x) x$rhs$value),
                  vcapply(cfg, function(x) x$lhs$name_target))
  ## Here, we do need to check a bunch of values, really.
  defaults <- list(base="odin", include=character(0))
  if (obj$file != "") {
    defaults$base <- gsub("[-.]", "_", basename_no_ext(obj$file))
  }

  ## These all need support on the C side.
  ##
  ## Consider namespacing these here:
  ## atol=formals(deSolve::lsoda)$atol,
  ## rtol=formals(deSolve::lsoda)$rtol,
  ## method="lsoda", # same as deSolve::ode
  ## ## delay only
  ## mxhist=10000
  ##
  ## Also allow renaming here (time, derivs, etc).

  err <- setdiff(names(dat), names(defaults))
  if (length(err)) {
    tmp <- cfg[match(err, names(dat))]
    odin_error(sprintf("Unknown configuration options: %s",
                       paste(err, collapse=", ")),
               get_lines(tmp), get_exprs(tmp))
  }

  ## Now, we need to typecheck these.  This is really annoying to do!
  char <- vlapply(defaults, is.character)
  ok <- vlapply(dat, is.character) == char[names(dat)]
  if (!all(ok)) {
    ## TODO: better error messages (which are the wrong types?)
    ##
    ## TODO: this branch is not tested (there was a typo in which that
    ## is not triggered anywhere)
    tmp <- cfg[!ok]
    odin_error(sprintf("Incorrect type (char vs numeric) for configuration: %s",
                       paste(names(which(!ok)), collapse=", ")),
               get_lines(tmp), get_exprs(tmp))
  }

  i <- names(dat) == "include"
  if (any(i)) {
    read <- function(j) {
      tryCatch(read_user_c(dat[[j]]),
               error=function(e)
                 odin_error(paste("Could not read include file:", e$message),
                            cfg[i][[j]]$line,
                            cfg[i][[j]]$expr))
    }
    res <- lapply(which(i), read)
    res <- list(declarations=unlist(lapply(res, "[[", "declarations")),
                definitions=unlist(lapply(res, "[[", "definitions")))
    if (any(duplicated(res$declarations))) {
      ## TODO: could be nicer
      stop("Duplicate declarations while reading includes")
    }
    dat <- dat[-i]
    dat$include <- res
  }

  obj$config <- modifyList(defaults, dat)
  if (!grepl("^[[:alnum:]_]+$", obj$config$base)) {
    stop(sprintf("Invalid base value: '%s', must contain letters, numbers and underscores only", obj$config$base))
  }

  if (length(obj$config$include) == 0L) {
    obj$config$include <- NULL
  }

  keep <- !obj$traits[, "is_config"]
  obj$eqs <- obj$eqs[keep]
  obj$traits <- obj$traits[keep, , drop=FALSE]
  obj
}

## So the pair:
##   x[1] <- ...
##   x[2:n] <- ...
## will get grouped together as a single x.  All dependencies of the
## expression will be combined, and the source reference also gets
## updated.
odin_parse_combine_arrays <- function(obj) {
  eqs <- obj$eqs
  is_dim <- vlapply(eqs, function(x) identical(x$lhs$special, "dim"))
  is_deriv <- vlapply(eqs, function(x) identical(x$lhs$special, "deriv"))
  is_initial <- vlapply(eqs, function(x) identical(x$lhs$special, "initial"))
  is_array <- vcapply(eqs, function(x) x$lhs$type) == "array"

  nms <- vcapply(eqs, "[[", "name")

  nms_real <- nms
  i <- is_deriv | is_initial | is_dim
  nms_real[i] <- vcapply(eqs[i], function(x) x$lhs$name_target)
  names(nms_real) <- nms

  err <- intersect(nms_real[is_array & !is_dim],
                   nms_real[!is_array & !is_dim])
  if (length(err) > 0L) {
    i <- nms_real %in% err
    line <- viapply(eqs[i], "[[", "line")
    expr <- as.expression(lapply(eqs[i], "[[", "expr"))
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
    line <- viapply(eqs[i], "[[", "line")
    expr <- as.expression(lapply(eqs[i], "[[", "expr"))
    odin_error(sprintf("Duplicate entries must all be arrays (%s)",
                       paste(err, collapse=", ")),
               line, expr)
  }

  ## Now, work through the dim() calls so we establish dimensionality
  ## of arrays.
  nd <- setNames(viapply(eqs[is_dim], check_dim_rhs), nms_real[is_dim])
  if (any(is.na(nd))) {
    i <- which(is_dim)[is.na(nd)]
    odin_error("Invalid dim() rhs", get_lines(eqs[i]), get_exprs(eqs[i]))
  }
  if (any(nd == 0L)) {
    ## Then, check for user-driven array sizes; we'll pull nd from the
    ## definition.
    i <- nd == 0L
    j <- match(nms_real[is_dim][i], nms)
    ## First check that the underlying variable is a user-sized array.
    err <- !vlapply(eqs[j], function(x) x$rhs$user)
    if (any(err)) {
      odin_error("user-specified dim() must be used with user-specified array",
                 get_lines(eqs[j][err]), get_exprs(eqs[j][err]))
    }
    err <- !vlapply(eqs[j], function(x) identical(x$lhs$type, "array"))
    if (any(err)) {
      odin_error("user-specified dim() must be used with array",
                 get_lines(eqs[j][err]), get_exprs(eqs[j][err]))
    }

    ## TODO: It's possible here that we'll get duplicated calls to
    ## user; they'll probably make it through here which won't cause a
    ## great problem but it's a bit ugly, and hard to check.
    nd[i] <- viapply(eqs[j], function(x) x$lhs$nd)
  }

  j <- which(is_dim)
  for (i in seq_along(nd)) {
    eqs[[j[[i]]]]$nd <- nd[[i]]
  }

  ## Then, work out which sets to combine
  i <- match(nms, unique(nms[is_array]))
  i <- unname(split(which(!is.na(i)), na.omit(i)))

  for (j in i) {
    k <- j[[1]]
    x <- eqs[[k]]
    x$depends <-
      join_deps(lapply(eqs[j], function(x) x[["depends"]]))
    ## NOTE: this is the only case where self referential variables
    ## are allowed.  There's no checking here and things like
    ##   x[i] = x[i] * 2
    ## will cause a crash or nonsense behaviour.
    x$depends$variables <- setdiff(x$depends$variables, x$name)
    if (!is_dim[k]) {
      x$depends$variables <- union(x$depends$variables,
                                   sprintf("dim_%s", nms_real[[x$name]]))
    }
    x$expr <- lapply(eqs[j], "[[", "expr")
    x$line <- viapply(eqs[j], "[[", "line")

    nd_x <- tryCatch(nd[[nms_real[[x$name]]]],
                     error=function(e) {
                       odin_error(
                         sprintf("No dim() call found for %s - used in:",
                                 x$lhs$name_target %||% x$name),
                         x$line, as.expression(x$expr))})

    err <- viapply(eqs[j], function(x) x[["lhs"]][["nd"]]) != nd_x
    if (any(err)) {
      odin_error(
        sprintf("Array dimensionality is not consistent (expected %d %s)",
                nd_x, ngettext(nd_x, "index", "indices")),
        get_lines(eqs[j]), get_exprs(eqs[j]))
    }

    ## TODO: some of the lhs depends stuff will not matter so much now.
    x$lhs$index <- lapply(eqs[j], function(x) x[["lhs"]][["index"]])
    x$lhs$depends <-
      join_deps(lapply(eqs[j], function(x) x[["lhs"]][["depends"]]))

    x$rhs$type <- vcapply(eqs[j], function(x) x[["rhs"]][["type"]])
    x$rhs$depends <-
      join_deps(lapply(eqs[j], function(x) x[["rhs"]][["depends"]]))
    x$rhs$value <- lapply(eqs[j], function(x) x[["rhs"]][["value"]])

    ## TODO: All these sanity checks need major overhauls, I think.
    ## Sanity check:
    ok <- c("name", "lhs", "rhs", "depends", "expr", "line")
    stopifnot(length(setdiff(unlist(lapply(eqs[j], names)), ok)) == 0L)
    ## NOTE: mixed type specials are dealt with elsewhere.  By this I
    ## mean that a variable is more than one of initial(), deriv(),
    ## output() and plain.
    used_lhs <- unlist(lapply(eqs[j], function(x) names(x$lhs)))
    ok <- c("type", "name", "name_target", "index", "nd", "depends", "special")
    stopifnot(length(setdiff(used_lhs, ok)) == 0L)

    used_rhs <- unlist(lapply(eqs[j], function(x) names(x$rhs)))
    if ("delay" %in% used_rhs) {
      if (length(j) > 1L) {
        odin_error("delay() may only be used on a single-line array assignment",
                   get_lines(eqs[j]), get_exprs(eqs[j]))
      }
    } else if ("user" %in% used_rhs) {
      if (length(j) > 1L) {
        odin_error("user() may only be used on a single-line array assignment",
                   get_lines(eqs[j]), get_exprs(eqs[j]))
      }
    } else {
      ## If a delay or a user value is used, then we _must_ have only a
      ## single thing here.  So we'll check these separately.
      ok <- c("type", "depends", "value", "user", "default",
              "interpolate", "interpolate_data", "sum")
      stopifnot(length(setdiff(used_rhs, ok)) == 0L)
    }
    eqs[[k]] <- x
  }

  drop <- unlist(lapply(i, "[", -1L))
  if (length(drop) > 0L) {
    eqs <- eqs[-drop]
    nms_real <- nms_real[-drop]
  }

  obj$eqs <- eqs
  obj$has_array <- any(is_array)
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
odin_parse_rewrite_initial_conditions <- function(obj) {
  vars <- obj$vars
  i <- vlapply(obj$eqs, function(x) (identical(x$lhs$special, "initial") &&
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
    obj$eqs[i] <- lapply(obj$eqs[i], f)
  }
  obj
}

odin_parse_dependencies <- function(obj) {
  eqs <- obj$eqs
  vars <- obj$vars
  nms <- vcapply(eqs, "[[", "name")
  exclude <- c("", INDEX)

  ## Array delay variables need to delay on the dimensions of their
  ## "present" array, so that the order of initialisation is always
  ## correct.  In practice I don't think this is a big deal because
  ## array sizing is not time dependent.  However, this resolves a
  ## difficulty in determining the total array size of the delay
  ## bookkeeping indices.
  is_delay <- vlapply(eqs, function(x) isTRUE(x$rhs$delay))
  is_array <- setNames(vlapply(eqs, function(x) identical(x$lhs$type, "array")),
                       nms)
  if (any(is_delay)) {
    for (i in which(is_delay)) {
      j <- is_array[setdiff(eqs[[i]]$rhs$depends_delay$variables,
                            c(obj$vars, INDEX))]
      if (any(j)) {
        eqs[[i]]$rhs$depends$variables <-
                 union(eqs[[i]]$rhs$depends$variables,
                       vcapply(names(j[j]), array_dim_name, USE.NAMES=FALSE))
      }
    }
  }

  deps <- lapply(eqs, function(el) setdiff(el$depends$variables, exclude))
  names(deps) <- nms

  msg <- lapply(deps, setdiff, c(nms, vars, TIME))
  i <- lengths(msg) > 0L
  if (any(i)) {
    odin_error(sprintf("Unknown variables %s",
                       paste(sort(unique(unlist(msg))), collapse=", ")),
               get_lines(eqs[i]), get_exprs(eqs[i]))
  }

  ## For the derivative calculations the variables come in with no
  ## dependencies because they are provided by the integrator, but
  ## we'll add an implicit time dependency.
  time_implicit <- paste0(TIME, "<implicit>")
  dummy <- c(list(t=character(0)),
             setNames(list(character(0)), time_implicit),
             setNames(rep(list(time_implicit), length(vars)), vars))
  order <- topological_order(c(deps, dummy))

  ## Then, we work out the recursive dependencies; this is the entire
  ## dependency chain of a thing; including its dependencies, its
  ## dependencies dependencies and so on.
  deps_rec <- recursive_dependencies(order, c(deps, dummy), vars)
  is_delay <- vlapply(eqs, function(x) isTRUE(x$rhs$delay))
  is_deriv <- vlapply(eqs, function(x) identical(x$lhs$special, "deriv"))
  is_initial <- vlapply(eqs, function(x) identical(x$lhs$special, "initial"))
  is_output <- vlapply(eqs, function(x) identical(x$lhs$special, "output"))
  is_user <- vlapply(eqs, function(x) isTRUE(x$rhs$user))
  is_interpolate <- vlapply(eqs, function(x) isTRUE(x$rhs$interpolate))

  ## Then, we can get the stage for these variables:
  stage <- setNames(rep(STAGE_CONSTANT, length(order)), order)
  stage[c(TIME, time_implicit)] <- STAGE_TIME
  stage[nms[is_user]] <- STAGE_USER
  stage[nms[is_delay | is_output | is_deriv | is_interpolate]] <- STAGE_TIME

  ## OK, this is potentially quite nasty for initial values that
  ## depend on time, on delay equations, etc, because it requires
  ## pulling a whole extra set of calculations out like we do for the
  ## delays.

  ## In topological order, determine inherited stage (a initial/time stage
  ## anywhere in a chain implies a initial/time stage).
  for (i in seq_along(order)) {
    stage[[i]] <- max(stage[[i]], stage[deps_rec[[i]]])
  }

  ## Lots of ugly processing here.  The idea here is to order the
  ## time-dependent variables correctly within each delay block and to
  ## filter out any non-time-dependent things.
  if (any(is_delay)) {
    delay_arrays <- collector()
    f <- function(x) {
      tmp <- setdiff(x$rhs$depends_delay$variables, INDEX)
      deps <- unique(c(tmp, unlist(deps_rec[tmp], use.names=FALSE)))
      ## NOTE: We have to exclude delayed values from the dependencies
      ## here, even though they are time dependent (*different* sort
      ## of time dependence...)
      deps <- setdiff(deps[stage[deps] == STAGE_TIME],
                      c(TIME, time_implicit, nms[is_delay]))
      delay_arrays$add(intersect(names(which(is_array)), deps))
      deps[order(match(deps, order))]
    }
    for (i in which(is_delay)) {
      eqs[[i]]$rhs$order_delay <- f(eqs[[i]])
    }
    tmp <- unique(delay_arrays$get())
    obj$delay_arrays <- setNames(sprintf("delay_%s", tmp), tmp)
  }

  if (any(stage[nms[is_initial]] == STAGE_TIME)) {
    ## NOTE: The intersect() here ensures correct ordering.
    initial_t <- names(which(stage[nms[is_initial]] == STAGE_TIME))
    tmp <- unique(unlist(deps_rec[initial_t], use.names=FALSE))
    tmp <- intersect(order, setdiff(tmp[stage[tmp] == STAGE_TIME], TIME))
    obj$initial_t_deps <- tmp
  }

  ## Adjust the order so that it's by stage first, and then the order.
  ## This should not create any impossible situations because of the
  ## stage treatent above.
  i <- order(stage)
  stage <- stage[i]
  order <- order[i]

  order_keep <- setdiff(order, names(dummy))

  ## Then, we compute two subgraphs (for dde) in the case where there
  ## are output variables.  We'd actually only want to get the output
  ## variables that are time dependent I think, but that really should
  ## be all of them.
  if (any(is_output)) {
    ## OK, what I need to find out here is:
    ##
    ##   * what is the full set of dependencies, including variables,
    ##     that are used in computing the output variables.
    ##
    ##   * what is *only* used in computing output variables
    ##
    ## This may change later...
    nms_output <- nms[is_output]
    used_output <-
      setdiff(unique(c(unlist(deps_rec[nms_output], use.names=FALSE),
                       nms_output)),
              c(TIME, time_implicit))
    used_output <- c(setdiff(used_output, order_keep),
                     intersect(order_keep, used_output))
    nms_deriv <- nms[is_deriv]
    used_deriv <- unique(c(unlist(deps_rec[nms_deriv], use.names=FALSE),
                           nms_deriv))

    if (any(is_delay)) {
      used_delay <-
        unique(unlist(lapply(eqs[is_delay], function(x) x$rhs$order_delay)))
    } else {
      used_delay <- character(0)
    }

    only_output <- setdiff(used_output, c(used_deriv, used_delay))
    only_output <- intersect(only_output, names(stage[stage == STAGE_TIME]))

    output_info <- list(used=used_output, only=only_output, deriv=used_deriv)
    stage[only_output] <- STAGE_OUTPUT
  } else {
    output_info <- NULL
  }

  ## TODO: Special treatment is needed for time-dependent initial
  ## conditions; they get special treatment and are held to max of
  ## STAGE_USER.  However, we'll record that they are time-dependent
  ## here.
  is_initial <- vlapply(eqs, function(x) identical(x$lhs$special, "initial"))
  initial_stage <- (if (any(is_delay)) STAGE_TIME
                    else max(c(STAGE_CONSTANT, stage[nms[is_initial]])))

  is_dim <- vlapply(eqs, function(x) identical(x$lhs$special, "dim"))
  dim_stage <- max(c(STAGE_CONSTANT, stage[nms[is_dim]]))

  ## Check for unused branches:
  is_deriv <- vlapply(eqs, function(x) identical(x$lhs$special, "deriv"))
  endpoints <- nms[is_initial | is_output | is_deriv]
  used <- union(endpoints, unique(unlist(deps_rec[endpoints], use.names=FALSE)))
  unused <- setdiff(nms, used)
  if (length(unused) > 0L) {
    i <- sort(match(unused, nms))
    ## TODO: this is disabled for now because it breaks a ton of tests
    ## that I want to throw earlier.
    ##
    ## TODO: perhaps this should be a warning?
    ## odin_error(sprintf("Unused variables: %s", paste(unused, collapse=", ")),
    ##            get_lines(eqs[i]), get_exprs(eqs[i]))
  }

  ## We need to filter the dimensions off here for user arrays:
  is_user <- is_user & !is_dim
  user <- setNames(!vlapply(eqs[is_user], function(x) x$rhs$default),
                   nms[is_user])

  ## NOTE: Be careful doing anything after the re-order as you will
  ## probably make a mistake; this invalidates most variables really.
  i <- match(order_keep, nms)
  eqs <- eqs[i]
  nms <- nms[i]
  deps <- deps[i]
  deps_rec <- deps_rec[i]
  names(eqs) <- nms

  for (i in nms) {
    eqs[[i]]$stage <- stage[[i]]
  }

  ## TODO: The other thing that is needed through here is going to be
  ## information about _exactly_ which variables need unpacking from
  ## the structs; there will be models where time is never
  ## _explicitly_ used (the lorenz attractor is one such model).
  ## There will be models where some variables have derivatives
  ## computed but the value of the variable is never referenced in the
  ## calculations and there will be (in the dde/output case) variables
  ## that don't need unpacking.  I think that if I change the dummy
  ## 't' variable to be '<time>' and use that for detecting stage but
  ## look out for an explicit time variable that would be preferable.
  obj$eqs <- eqs
  obj$user <- user
  obj$initial_stage <- initial_stage
  obj$dim_stage <- dim_stage
  obj$output_info <- output_info
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
  err <- intersect(index_vars, names(which(is_array)))
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
  is_array <- setNames(vlapply(obj$eqs[sprintf("deriv_%s", vars)],
                               function(x) x$lhs$type == "array"), vars)

  ord <- rep(0L, length(is_array))
  array <- setNames(rep(0L, length(is_array)), vars)
  if (any(is_array)) {
    tmp <- vcapply(vars[is_array], array_dim_name, USE.NAMES=FALSE)
    ord[is_array] <- match(tmp, names(obj$eqs))
    array[is_array] <- viapply(obj$eqs[tmp], "[[", "nd")
  }

  vars <- vars[order(ord)]
  is_array <- is_array[vars]
  array <- array[vars]

  stage <- rep(STAGE_CONSTANT, length(is_array))
  stage[is_array] <- viapply(obj$eqs[sprintf("dim_%s", vars[is_array])],
                             function(x) x$stage)
  names(stage) <- vars

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

  ## Check whether variables are actually used in the time equations:
  used <- vars %in% unlist(lapply(obj$eqs, function(x)
    if (x$stage >= STAGE_TIME) x$depends$variables), use.names=FALSE)
  names(used) <- vars

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
  is_delay <- which(vlapply(obj$eqs, function(x) isTRUE(x$rhs$delay)))
  obj$has_delay <- length(is_delay) > 0L
  if (!obj$has_delay) {
    return(obj)
  }

  for (idx in seq_along(is_delay)) {
    i <- is_delay[[idx]]
    x <- obj$eqs[[i]]
    deps <- x$rhs$order_delay
    time <- x$rhs$value_time
    if (is.recursive(time)) {
      time <- call("(", time)
    }
    ## TODO: Consider checking through the time values and making sure
    ## we don't include any INDEX variables.  Later they will be
    ## supported.  I think that time is OK though.
    if (any(INDEX %in% x$rhs$depends$variables)) {
      odin_error("delay times may not reference index variables (yet)",
                 x$line, x$expr)
    }
    ## TODO: check for delay through the recursive deps for delay vars...

    ## Here, it's really important to pull these out with the scalars
    ## first, then the arrays.
    vars_is_array <- obj$variable_order$is_array
    extract <- intersect(names(vars_is_array), deps) # retains ordering
    is_array <- vars_is_array[extract]
    len <- length(extract)
    size <- vector("list", len)
    offset <- vector("list", len)
    for (j in seq_len(len)) {
      if (!is_array[[j]]) {
        size[[j]] <- 1L
        offset[[j]] <- j - 1L
      } else {
        size[[j]] <- array_dim_name(extract[[j]])
        if (j == 1L || !is_array[[j - 1L]]) {
          offset[[j]] <- j - 1L
        } else {
          offset[[j]] <- size[[j - 1L]]
        }
      }
    }
    names(size) <- names(offset) <- extract

    deps <- setdiff(deps, extract)
    dep_is_array <- vcapply(obj$eqs[deps], function(x) x$lhs$type) == "array"

    obj$eqs[[i]]$delay <- list(idx=idx,
                               time=time,
                               extract=extract,
                               deps=deps,
                               dep_is_array=dep_is_array,
                               is_array=is_array,
                               size=size,
                               offset=offset)
  }

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

check_dim_rhs <- function(x) {
  if (x$rhs$type == "atomic" || is.name(x$rhs$value)) {
    1L
  } else if (is_call(x$rhs$value, quote(c))) {
    ok <- vlapply(as.list(x$rhs$value[-1L]),
                  function(x) is.symbol(x) || is.atomic(x))
    if (!all(ok)) {
      NA_integer_
    } else {
      length(ok)
    }
  } else if (isTRUE(x$rhs$user)) {
    if (isTRUE(x$rhs$default)) {
      odin_error("Default in user dimension size not handled",
                 x$line, x$expr)
    }
    0L
  } else {
    NA_integer_
  }
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

odin_parse_process_interpolate <- function(obj) {
  ## TODO: make this more like delay where we do isTRUE(x$rhs$interpolate)
  is_interpolate <-
    vlapply(obj$eqs, function(x) "interpolate" %in% x$depends$functions)
  obj$has_interpolate <- any(is_interpolate)
  if (!obj$has_interpolate) {
    return(obj)
  }

  nms <- vcapply(obj$eqs, function(x) x$name)

  ## Here, we need to go through and inspect all the variables that
  ## are used in interpolation.  The way this works is we're going
  ## add one rank to the input, require a few sizes here and there.
  ## It's going to be a blast!
  ##
  ## The two variables will be declared as _user_ stage variables.
  f <- function(e) {
    expr <- e$rhs$value
    rank <- if (e$lhs$type == "symbol") 0L else e$lhs$nd

    ## Already checked that 't' and 'y' are symbols...
    g <- function(sym, rank) {
      nm <- as.character(sym)
      j <- match(nm, nms)
      if (is.na(j)) {
        odin_error(sprintf("Interpolation variable %s not found", nm),
                   e$line, e$expr)
      }
      target <- obj$eqs[[j]]
      if (target$lhs$type != "array" || target$lhs$nd != rank) {
        type <- if (rank == 1L) "vector" else paste(rank, "dimensional array")
        odin_error(sprintf("Expected %s to be a %s", nm, type),
                   e$line, e$expr)
      }
      ## This requires some serious work because it requires that the user
      ## supplied 'y' is *four* dimensional.  It can probably be treated
      ## somewhat specially though, but I fear it will make a mess of
      ## various checks.
      if (rank > 3) {
        odin_error("interpolating 3d arrays not yet supported",
                   e$line, e$expr)
      }
      nm
    }
    nm_t <- g(expr[[2]], 1L)
    nm_y <- g(expr[[3]], rank + 1L)
    e$rhs$interpolate_data <-
      list(type=expr[[4]],
           nd=rank,
           ny=if (rank == 0L) 1L else array_dim_name(nm_y),
           nt=array_dim_name(nm_t),
           t=nm_t,
           y=nm_y,
           name=paste0("interpolate_", e$name))
    e$rhs$interpolate <- TRUE
    e
  }

  obj$eqs[is_interpolate] <- lapply(obj$eqs[is_interpolate], f)

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
