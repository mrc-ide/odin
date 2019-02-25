odin_build_ir2 <- function(x, validate = FALSE, pretty = TRUE) {
  ## TODO: this gets looked at later
  xp <- odin_preprocess(x)

  ## this just checks that everything is ok; we can examine this in
  ## more detail later but because it does not affect the output it
  ## can just be run as is for now.
  odin_parse_prepare(xp$exprs)

  dat <- ir_parse_exprs(xp$exprs)
  eqs <- dat$eqs
  source <- dat$source

  ## TODO: config not yet done
  ## TODO: initial rewrite should be done with substitutions instead
  ## TODO: array combination

  ## Data elements:

  features <- ir_parse_features(eqs)

  variables <- ir_parse_find_variables(eqs, features$discrete)

  if (features$has_array) {
    eqs <- ir_parse_arrays(eqs, variables)
  }
  eqs <- eqs[order(names(eqs))]

  meta <- ir_parse_meta(features$discrete)
  config <- ir_parse_config()

  ## If we have arrays, then around this point we will also be
  ## generating a number of additional offset and dimension length
  ## variables.  So watch for the "data" element to have extra return
  ## objects perhaps.

  dependencies <- ir_parse_dependencies(eqs, variables, meta$time)
  stage <- ir_parse_stage(eqs, dependencies, variables, meta$time)

  eqs_initial <- names_if(vlapply(eqs, function(x)
    identical(x$lhs$special, "initial")))
  features$initial_time_dependent <-
    features$has_delay || max(stage[eqs_initial]) == STAGE_TIME

  data <- ir_parse_data(eqs, variables, stage)

  if (features$has_user) {
    is_user <- vcapply(eqs, "[[", "type") == "user"
    user <- unname(lapply(eqs[is_user], function(x)
      list(name = x$name, has_default = !is.null(x$user$default))))
    user <- user[order(vlapply(user, "[[", "has_default"))]
  } else {
    user <- list()
  }

  interpolate <- list(min = list(), max = list(), critical = list())

  components <- ir_parse_components(eqs, dependencies, variables, stage,
                                    features$discrete)
  equations <- ir_parse_equations(eqs)

  ret <- list(config = config,
              meta = meta,
              features = features,
              data = data,
              equations = equations,
              components = components,
              user = user,
              interpolate = interpolate,
              source = source)
  ir <- ir_serialise(ret, pretty)
  if (validate) {
    ir_validate(ir, TRUE)
  }
  ir
}


ir_parse_data <- function(eqs, variables, stage) {
  type <- vcapply(eqs, function(x) x$type, USE.NAMES = FALSE)
  i <- !(type %in% c("alloc", "alloc_interpolate", "alloc_ring", "copy"))
  elements <- lapply(eqs[i], ir_parse_data_element, stage)
  names(elements) <- vcapply(elements, "[[", "name")
  ## For ease of comparison:
  elements <- elements[order(names(elements))]

  pack_variables <- ir_parse_packing(variables, elements, TRUE)

  output <-
    c(vcapply(eqs[type == "copy"], function(x)
      x$lhs$name_data, USE.NAMES = FALSE),
      names_if(vcapply(elements, "[[", "location") == "output"))
  pack_output <- ir_parse_packing(output, elements, FALSE)

  list(elements = elements,
       variable = pack_variables,
       output = pack_output)
}


ir_parse_data_element <- function(x, stage) {
  name <- x$lhs$name_data

  storage_type <- x$lhs$storage_mode %||% "double"
  if (is.null(x$array)) {
    rank <- 0L
    dimnames <- NULL
  } else {
    rank <- x$array$rank
    dimnames <- x$array$dimnames
  }

  if (is.null(x$lhs$special)) {
    if (rank == 0L && stage[[x$name]] == STAGE_TIME) {
      location <- "transient"
    } else {
      location <- "internal"
    }
  } else if (x$lhs$special == "initial") {
    location <- "internal"
    name <- x$lhs$name_equation
  } else if (x$lhs$special == "deriv" || x$lhs$special == "update") {
    location <- "variable"
  } else if (x$lhs$special == "output") {
    location <- "output"
  } else {
    stop("odin bug?")
  }

  list(name = name,
       location = location,
       storage_type = storage_type,
       rank = rank,
       dimnames = dimnames)
}


ir_parse_meta <- function(discrete) {
  time <- if (discrete) STEP else TIME
  result <- if (discrete) STATE_NEXT else DSTATEDT
  list(internal = INTERNAL,
       user = USER,
       state = STATE,
       result = result,
       output = OUTPUT,
       time = time,
       initial_time = initial_name(time))
}


ir_parse_config <- function() {
  list(base = "odin")
}


ir_parse_find_variables <- function(eqs, discrete) {
  is_special <- vlapply(eqs, function(x) !is.null(x$lhs$special))
  special <- vcapply(eqs[is_special], function(x) x$lhs$special)
  name_data <- vcapply(eqs[is_special], function(x) x$lhs$name_data)
  rhs_fun <- if (discrete) "update" else "deriv"
  is_initial <- special == "initial"
  is_var <- special == rhs_fun

  ## Extract the *real* name here:
  vars <- name_data[is_var]
  vars_initial <- name_data[is_initial]

  if (!setequal(vars, vars_initial)) {
    msg <- collector()
    msg_initial <- setdiff(vars, vars_initial)
    if (length(msg_initial) > 0L) {
      msg$add("\tin %s() but not initial(): %s",
              rhs_fun, paste(msg_initial, collapse = ", "))
    }
    msg_vars <- setdiff(vars_initial, vars)
    if (length(msg_vars) > 0L) {
      msg$add("\tin initial() but not %s(): %s",
              rhs_fun, paste(msg_vars, collapse = ", "))
    }
    tmp <- eqs[is_var | is_initial]
    odin_error(sprintf(
      "%s() and initial() must contain same set of equations:\n%s\n",
      rhs_fun, paste(msg$get(), collapse = "\n")),
      get_lines(tmp), get_exprs(tmp))
  }

  err <- names(is_var) %in% vars
  if (any(err)) {
    odin_error(
      sprintf("variables on lhs must be within %s() or initial() (%s)",
              rhs_fun,
              paste(intersect(vars, names(eqs)), collapse = ", ")),
      get_lines(eqs[err]), get_exprs(eqs[err]))
  }

  unique(unname(vars))
}


ir_parse_dependencies <- function(eqs, variables, time_name) {
  ## For the derivative calculations the variables come in with no
  ## dependencies because they are provided by the integrator (but
  ## we'll add an implicit time dependency).
  implicit <- set_names(rep(list(character(0)), length(variables) + 1),
                        c(variables, time_name))
  explicit <- lapply(eqs, function(eq)
    setdiff(eq$depends$variables, c(eq$name, INDEX, "")))
  deps <- c(explicit, implicit)

  msg <- lapply(deps, setdiff, names(deps))
  i <- lengths(msg) > 0L
  if (any(i)) {
    msg <- sort(unique(unlist(msg)))
    ## TODO: this is not *variable* as such.
    fmt <- ngettext(length(msg), "Unknown variable %s",  "Unknown variables %s")
    odin_error(sprintf(fmt, paste(msg, collapse = ", ")),
               get_lines(eqs[i]), get_exprs(eqs[i]))
  }

  order <- topological_order(deps)

  ## Then, we work out the recursive dependencies; this is the entire
  ## dependency chain of a thing; including its dependencies, its
  ## dependencies dependencies and so on.
  recursive_dependencies(order, deps, variables)
}


ir_parse_stage <- function(eqs, dependencies, variables, time_name) {
  stage <- set_names(rep(STAGE_CONSTANT, length(dependencies)),
                     names(dependencies))
  is_user <- function(x) {
    x$type == "user"
  }
  is_null <- function(x) {
    x$type == "null"
  }
  is_time_dependent <- function(x) {
    (!is.null(x$lhs$special) &&
     x$lhs$special %in% c("deriv", "update", "output")) ||
      !is.null(x$rhs$delay) || !is.null(x$rhs$interpolate) ||
      isTRUE(x$rhs$stochastic)
  }

  stage[names_if(vlapply(eqs, is_user))] <- STAGE_USER
  stage[names_if(vlapply(eqs, is_time_dependent))] <- STAGE_TIME
  stage[time_name] <- STAGE_TIME
  stage[variables] <- STAGE_TIME

  ## In topological order, determine inherited stage (a initial/time stage
  ## anywhere in a chain implies a initial/time stage).
  for (i in seq_along(stage)) {
    stage[[i]] <- max(stage[[i]], stage[dependencies[[i]]])
  }

  ## NOTE: I am ignoring an old promotion of interpolation functions
  ## for the case where we use interpolation in a delay - see
  ## parse_dependencies.R for the details here.

  ## Any equation that has been removed because it is implied (this
  ## might be an issue only for the array cases).
  stage[names_if(vlapply(eqs, is_null))] <- STAGE_NULL

  stage
}


## TODO: once we support arrays, this will produce a set of offsets,
## and the stage will become quite important.  This calculation will
## need to be done before the stage and ordering is done ideally.
ir_parse_packing <- function(names, data, variables = FALSE) {
  rank <- viapply(data[names], "[[", "rank")

  ## We'll pack from least to most complex:
  i <- order(rank)
  names <- names[i]
  rank <- rank[i]

  len <- lapply(data[names], function(x) x$dimnames$length %||% 1L)
  is_array <- rank > 0L
  ## Accumulate offset and also total:
  offset <- vector("list", length(names) + 1L)
  offset[[1L]] <- 0L
  for (i in seq_along(names)) {
    if (!is_array[[i]]) {
      offset[[i + 1L]] <- i
    } else if (identical(offset[[i]], 0L)) {
      offset[[i + 1L]] <- as.name(len[[i]])
    } else {
      offset[[i + 1L]] <- call("+", offset[[i]], as.name(len[[i]]))
    }
  }

  ## Split those back apart
  length <- offset[[length(names) + 1L]]
  offset <- offset[seq_along(names)]

  ## Create auxillary offset variables
  if (any(vlapply(offset[seq_along(names)], is.call))) {
    stop("Add offsets")
  }

  ## And pack it all up:
  if (variables) {
    contents <- unname(Map(
      list, name = names, offset = offset, initial = initial_name(names)))
  } else {
    contents <- unname(Map(list, name = names, offset = offset))
  }

  list(length = length, contents = contents)
}


## TODO: this part will change.  Things that are set NULL here will be
## checked elsewhere and are set to NULL so that they can't be easily
## used elsewhere, and the IR validation will check that we've added
## them.
##
## A downside of the approach here is that we do make the checks in a
## few different places.  It might be worth trying to shift more of
## this classification into the initial equation parsing.
ir_parse_features <- function(eqs) {
  is_update <- vlapply(eqs, function(x) identical(x$lhs$special, "update"))
  is_deriv <- vlapply(eqs, function(x) identical(x$lhs$special, "deriv"))
  is_output <- vlapply(eqs, function(x) identical(x$lhs$special, "output"))
  is_dim <- vlapply(eqs, function(x) identical(x$lhs$special, "dim"))
  is_user <- vlapply(eqs, function(x) !is.null(x$user))
  is_delay <- vlapply(eqs, function(x) !is.null(x$delay))
  is_interpolate <- vlapply(eqs, function(x) !is.null(x$interpolate))
  is_stochastic <- vlapply(eqs, function(x) isTRUE(x$stochastic))

  if (any(is_update) && any(is_deriv)) {
    tmp <- eqs[is_deriv | is_update]
    odin_error("Cannot mix deriv() and update()",
               get_lines(tmp), get_exprs(tmp))
  }
  if (!any(is_update | is_deriv)) {
    odin_error("Did not find a deriv() or an update() call",
               NULL, NULL)
  }

  list(discrete = any(is_update),
       has_array = any(is_dim),
       has_output = any(is_output),
       has_user = any(is_user),
       has_delay = any(is_delay),
       has_interpolate = any(is_interpolate),
       has_stochastic = any(is_stochastic),
       initial_time_dependent = NULL)
}


ir_parse_components <- function(eqs, dependencies, variables, stage,
                                discrete) {
  eqs_constant <- intersect(names_if(stage == STAGE_CONSTANT), names(eqs))
  eqs_user <- intersect(names_if(stage == STAGE_USER), names(eqs))
  eqs_time <- intersect(names_if(stage == STAGE_TIME), names(eqs))

  ## NOTE: we need the equation name here, not the variable name
  rhs_special <- if (discrete) "update" else "deriv"
  rhs <- names_if(vlapply(eqs, function(x)
    identical(x$lhs$special, rhs_special)))
  v <- unique(unlist(dependencies[rhs], use.names = FALSE))
  eqs_rhs <- intersect(eqs_time, c(rhs, v))
  variables_rhs <- intersect(variables, v)

  output <- names_if(vlapply(eqs, function(x)
    identical(x$lhs$special, "output")))
  v <- unique(c(character(), unlist(dependencies[output], use.names = FALSE)))
  eqs_output <- intersect(eqs_time, c(output, v))
  variables_output <- intersect(variables, v)

  initial <- names_if(vlapply(eqs, function(x)
    identical(x$lhs$special, "initial")))
  v <- unique(c(initial, unlist(dependencies[initial], use.names = FALSE)))
  eqs_initial <- intersect(eqs_time, v)

  list(
    create = list(variables = character(0), equations = eqs_constant),
    user = list(variables = character(0), equations = eqs_user),
    initial = list(variables = character(0), equations = eqs_initial),
    rhs = list(variables = variables_rhs, equations = eqs_rhs),
    output = list(variables = variables_output, equations = eqs_output))
}


ir_parse_exprs <- function(exprs) {
  lines <- utils::getSrcLocation(exprs, "line")
  src <- utils::getSrcref(exprs)

  ## If running odin::odin({...}) then R's parser does not add line
  ## numbers and source references.  However, these are going to be
  ## assumed later on, so we need to come up with something sensible
  ## now.  It might be better to offer to deparse these expressions
  ## through a formatter if available because R's deparser leads to
  ## some weird strings.
  if (is.null(lines)) {
    lines <- seq_along(exprs)
  }
  if (is.null(src)) {
    src <- vcapply(exprs, deparse_str)
  } else {
    src <- lapply(src, as.character)
    if (any(lengths(src) > 1)) {
      ## here we should update line numbers appropriately
      stop("Fix multiline expression")
    }
    src <- list_to_character(src)
  }

  eqs <- Map(ir_parse_expr, exprs, lines)
  names(eqs) <- vcapply(eqs, "[[", "name")
  list(eqs = eqs, source = src)
}


ir_parse_expr <- function(expr, line) {
  lhs <- ir_parse_expr_lhs(expr[[2L]], line, expr)
  rhs <- ir_parse_expr_rhs(expr[[3L]], line, expr)
  depends <- join_deps(list(lhs$depends, rhs$depends))
  rhs$depends <- NULL

  if (!is.null(rhs$user)) {
    type <- "user"
  } else if (identical(lhs$special, "dim")) {
    type <- "dim"
  } else if (lhs$type == "expression_scalar") {
    type <- "expression_scalar"
  } else if (lhs$type == "expression_array") {
    type <- "expression_array"
  } else {
    stop("writeme")
  }
  lhs$type <- NULL

  ## Below here uses both the lhs and rhs:
  if (type == "user") {
    if (!is.null(lhs$special) && !identical(lhs$special, "dim")) {
      odin_error("user() only valid for non-special variables", line, expr)
    }
  }

  ## This might actually be too strict because it's possible that dydt
  ## could be delayed dzdt but that seems unlikely.  Definitely cannot
  ## be most of the others.
  if (isTRUE(rhs$delay) && !is.null(lhs$special)) {
    odin_error("delay() only valid for non-special variables", line, expr)
  }

  if (identical(lhs$special, "output")) {
    is_copy <-
      isTRUE(rhs$rhs$value) || identical(rhs$rhs$value, as.name(lhs$name_data))
    if (is_copy) {
      type <- "copy"
      depends <- list(functions = character(0), variables = lhs$name_data)
      rhs <- NULL
    }
  }

  if (any(names(FUNCTIONS_INPLACE) %in% depends$functions)) {
    stop("check")
    rhs <- odin_parse_expr_rhs_rewrite_inplace(rhs, lhs, line, expr)
  }

  ## NOTE: arrays are the only case where self referential variables
  ## are allowed.  For arrays, there's no checking here and things like
  ##   x[i] = x[i] * 2
  ## will cause a crash or nonsense behaviour.
  ##
  ## TODO: look at this carefully; the rule for derivatives has been
  ## bodged in here.
  is_self_ref <- lhs$name_data %in% depends$variables &&
    type != "expression_array" &&
    !identical(lhs$special, "deriv") &&
    !identical(lhs$special, "update") &&
    type != "copy"
  if (is_self_ref) {
    odin_error("Self referencing expressions not allowed (except for arrays)",
               line, expr)
  }

  ret <- list(name = lhs$name_equation,
              type = type,
              lhs = lhs,
              depends = depends,
              source = line)
  c(ret, rhs)
}


ir_parse_expr_lhs <- function(lhs, line, expr) {
  is_special <- is_array <- FALSE
  special <- index <- depends <- NULL

  if (is.call(lhs)) {
    fun <- deparse_str(lhs[[1L]])
    if (fun %in% SPECIAL_LHS) {
      if (length(lhs) != 2L) {
        odin_error("Invalid length special function on lhs", line, expr)
      }
      is_special <- TRUE
      special <- fun
      lhs <- lhs[[2L]]
    }
  }

  if (is_call(lhs, "[")) {
    if (is_special && special == "dim") {
      odin_error("dim() must be applied to a name only (not an array)",
                 line, expr)
    }
    is_array <- TRUE
    tmp <- ir_parse_expr_lhs_index(lhs, line, expr)
    index <- tmp$index
    depends <- tmp$depends
    lhs <- lhs[[2L]]
  }

  name <- ir_parse_expr_check_lhs_name(lhs, line, expr)
  type <- if (is_array) "expression_array" else "expression_scalar"

  ## name_equation: the name we'll use for the equation
  ## name_data: the name of the data element
  ## name_lhs: the name of the lhs of the assignment (possibly always the
  ##   same as name_equation?)
  name_data <- name
  name_equation <- if (is_special) sprintf("%s_%s", special, name) else name

  if (!is_special) {
    name_lhs <- name_equation
  } else if (special %in% c("initial", "dim")) {
    name_lhs <- name_equation
  } else if (special %in% c("deriv", "output", "update")) {
    name_lhs <- name_data
  } else {
    stop("checkme")
  }

  list(type = type,
       name_data = name_data,
       name_equation = name_equation,
       name_lhs = name_lhs,
       special = special,
       index = index,
       depends = depends)
}


ir_parse_expr_lhs_index <- function(lhs, line, expr) {
  if (!is.name(lhs[[2L]])) {
    odin_error("array lhs must be a name", line, expr)
  }

  index <- as.list(lhs[-(1:2)])

  is_empty <- vlapply(index, identical, quote(expr = ))
  ## TODO: it might be useful to treat these specially rather than
  ## filling them in like this.
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

  list(
    index = Map(list, value = index, is_range = is_range,
                index = INDEX[seq_along(index)]),
    depends = deps)
}


ir_parse_expr_check_lhs_name <- function(lhs, line, expr) {
  ## at this point there are lots of other corner cases; things like
  ## nested special functions.
  if (is.call(lhs)) {
    odin_error(sprintf("Unhandled expression %s on lhs", fun), line, expr)
  }

  ## things like atomic will raise here: 1 <- 2
  if (!is.name(lhs)) {
    odin_error("Invalid left hand side", line, expr)
  }

  name <- deparse(lhs)

  if (name %in% RESERVED) {
    odin_error("Reserved name for lhs", line, expr)
  }
  re <- sprintf("^(%s)_.*", paste(RESERVED_PREFIX, collapse = "|"))
  if (grepl(re, name)) {
    odin_error(sprintf("Variable name cannot start with '%s_'",
                       sub(re, "\\1", name)),
               line, expr)
  }

  name
}


ir_parse_expr_rhs <- function(rhs, line, expr) {
  if (is_call(rhs, quote(delay))) {
    stop("writeme")
    return(odin_parse_expr_rhs_delay(rhs, line, expr))
  } else if (is_call(rhs, quote(user))) {
    ir_parse_expr_rhs_user(rhs, line, expr)
  } else if (is_call(rhs, quote(interpolate))) {
    stop("writeme")
    return(odin_parse_expr_rhs_interpolate(rhs, line, expr))
  } else {
    ir_parse_expr_rhs_expression(rhs, line, expr)
  }
}


ir_parse_expr_rhs_expression <- function(rhs, line, expr) {
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

  ## TODO: look at this later, but it's called only for throwing as
  ## side effect for now, so we can clean it up later easily
  odin_parse_expr_rhs_check_usage(rhs, line, expr)

  if ("sum" %in% depends$functions) {
    stop("rewriteme") # we've changed tack here
    rhs <- odin_parse_expr_rhs_rewrite_sum(rhs, line, expr)
    depends <- find_symbols(rhs)
  }

  if (":" %in% depends$functions) {
    odin_error("Range operator ':' may not be used on rhs", line, expr)
  }

  stochastic <- any(depends$functions %in% names(FUNCTIONS_STOCHASTIC))

  list(rhs = list(value = rhs),
       depends = depends,
       stochastic = stochastic)
}


ir_parse_expr_rhs_user <- function(rhs, line, expr) {
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
  ## TODO: the 'dim' part here is not actually known yet!
  user <- list(default = m$default,
               dim = FALSE,
               integer = m$integer,
               min = m$min,
               max = m$max)
  list(user = user)
}


ir_parse_equations <- function(eqs) {
  eqs <- eqs[vcapply(eqs, "[[", "type") != "null"]
  ## At some point we'll move this around
  f <- function(x) {
    x$lhs <- x$lhs$name_lhs
    x
  }
  eqs
}
