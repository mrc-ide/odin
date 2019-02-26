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

  ## Data elements:

  features <- ir_parse_features(eqs)

  variables <- ir_parse_find_variables(eqs, features$discrete)

  eqs <- lapply(eqs, ir_parse_rewrite_initial, variables)

  if (features$has_array) {
    eqs <- ir_parse_arrays(eqs, variables, source)
  }

  if (features$has_interpolate) {
    eqs <- ir_parse_interpolate(eqs, features$discrete, source)
    i <- vcapply(eqs, "[[", "type") == "alloc_interpolate"
    f <- function(v) {
      sort(unique(unlist(unname(lapply(eqs[i], function(eq)
        eq$control[[v]]))))) %||% character(0)
    }
    interpolate <-
      list(min = f("min"), max = f("max"), critical = f("critical"))
  } else {
    interpolate <- list(min = list(), max = list(), critical = list())
  }

  if (features$has_delay) {
    eqs <- ir_parse_delay(eqs, features$discrete, variables, source)
  }

  eqs <- eqs[order(names(eqs))]

  meta <- ir_parse_meta(features$discrete)
  ## TODO: determine the base here based on filenames
  config <- ir_parse_config(eqs, "odin")

  ## If we have arrays, then around this point we will also be
  ## generating a number of additional offset and dimension length
  ## variables.  So watch for the "data" element to have extra return
  ## objects perhaps.

  dependencies <- ir_parse_dependencies(eqs, variables, meta$time, source)
  stage <- ir_parse_stage(eqs, dependencies, variables, meta$time)

  eqs_initial <- names_if(vlapply(eqs, function(x)
    identical(x$lhs$special, "initial")))
  features$initial_time_dependent <-
    features$has_delay || max(stage[eqs_initial]) == STAGE_TIME

  data <- ir_parse_data(eqs, variables, stage, source)

  if (features$has_user) {
    is_user <- vcapply(eqs, "[[", "type") == "user"
    user <- unname(lapply(eqs[is_user], function(x)
      list(name = x$name, has_default = !is.null(x$user$default))))
    user <- user[order(vlapply(user, "[[", "has_default"))]
  } else {
    user <- list()
  }

  components <- ir_parse_components(eqs, dependencies, variables, stage,
                                    features$discrete)
  equations <- ir_parse_equations(eqs)

  ## TODO: it's a bit unclear where this best belongs
  ir_parse_check_functions(eqs, features$discrete, source)

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


ir_parse_data <- function(eqs, variables, stage, source) {
  type <- vcapply(eqs, function(x) x$type, USE.NAMES = FALSE)
  i <- !(type %in% c("alloc", "copy", "config"))
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

  err <- vlapply(eqs, function(x)
    identical(x$lhs$special, "output") && x$lhs$name_data %in% variables)
  if (any(err)) {
    ir_odin_error("output() name cannot be the same as variable name",
                  ir_get_lines(eqs[err]), source)
  }

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


ir_parse_config <- function(eqs, base_default) {
  i <- vcapply(eqs, "[[", "type") == "config"
  config <- unname(eqs[i])
  config_target <- vcapply(config, function(x) x$lhs$name_data)
  supported <- "base"

  i <- which(config_target == "base")
  if (length(i) == 0L) {
    ## TODO: pass in the default as arg
    base <- base_default
  } else if (length(i) == 1L) {
    base <- config[[i]]$rhs$value
  } else {
    stop("can't use base more than once")
  }

  err <- setdiff(config_target, supported)
  if (length(err) > 0L) {
    stop("unsupported configuration") # TODO: fix this up
  }

  list(base = base)
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


ir_parse_dependencies <- function(eqs, variables, time_name, source) {
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
    ir_odin_error(sprintf(fmt, paste(msg, collapse = ", ")),
                  ir_get_lines(eqs[i]), source)
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
      isTRUE(x$stochastic) || x$type == "delay_continuous"
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
  len <- lapply(data[names], function(x) x$dimnames$length %||% 1L)
  ir_parse_packing_internal(names, rank, len, variables)
}


ir_parse_packing_internal <- function(names, rank, len, variables = FALSE) {
  ## We'll pack from least to most complex:
  i <- order(rank)
  names <- names[i]
  rank <- rank[i]
  len <- len[i]

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
  src <- attr(exprs, "wholeSrcref", exact = TRUE)

  if (is.null(src)) {
    ## If running odin::odin({...}) then R's parser does not add line
    ## numbers and source references.  However, these are going to be
    ## assumed later on, so we need to come up with something sensible
    ## now.  It might be better to offer to deparse these expressions
    ## through a formatter if available because R's deparser leads to
    ## some weird strings.  There are clearly some ways of capturing
    ## the whole thing including comments and that would be really
    ## useful to look at.
    src <- vcapply(exprs, deparse_str)
    lines <- seq_along(exprs)
  } else {
    src <- as.character(src)
    lines0 <- utils::getSrcLocation(exprs, "line", first = TRUE)
    lines1 <- utils::getSrcLocation(exprs, "line", first = FALSE)
    lines <- Map(seq.int, lines0, lines1)
  }

  eqs <- Map(ir_parse_expr, exprs, lines, MoreArgs = list(source = src))
  names(eqs) <- vcapply(eqs, "[[", "name")
  list(eqs = eqs, source = src)
}


ir_parse_expr <- function(expr, line, source) {
  lhs <- ir_parse_expr_lhs(expr[[2L]], line, expr)
  rhs <- ir_parse_expr_rhs(expr[[3L]], line, expr, source)
  depends <- join_deps(list(lhs$depends, rhs$depends))
  rhs$depends <- NULL

  if (!is.null(rhs$user)) {
    type <- "user"
  } else if (!is.null(rhs$interpolate)) {
    type <- "interpolate"
  } else if (!is.null(rhs$delay)) {
    type <- "delay"
  } else if (identical(lhs$special, "dim")) {
    type <- "dim"
  } else if (identical(lhs$special, "config")) {
    type <- "config"
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
      ir_odin_error("user() only valid for non-special variables",
                    line, source)
    }
  }

  ## This might actually be too strict because it's possible that dydt
  ## could be delayed dzdt but that seems unlikely.  Definitely cannot
  ## be most of the others.
  if (type == "delay" && !is.null(lhs$special)) {
    ir_odin_error("delay() only valid for non-special variables",
                  line, source)
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
    ir_odin_error(
      "Self referencing expressions not allowed (except for arrays)",
      line, source)
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
  } else if (special %in% c("deriv", "output", "update", "config")) {
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


ir_parse_expr_rhs <- function(rhs, line, expr, source) {
  if (is_call(rhs, quote(delay))) {
    ir_parse_expr_rhs_delay(rhs, line, expr, source)
  } else if (is_call(rhs, quote(user))) {
    ir_parse_expr_rhs_user(rhs, line, expr)
  } else if (is_call(rhs, quote(interpolate))) {
    ir_parse_expr_rhs_interpolate(rhs, line, expr)
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
    rhs <- ir_parse_expr_rhs_expression_sum(rhs, line, expr)
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


ir_parse_expr_rhs_interpolate <- function(rhs, line, expr) {
  m <- match.call(function(t, y, type = "spine") NULL, rhs, FALSE)

  type <- m$type
  if (!is.character(type)) {
    odin_error("Expected a string constant for interpolation type",
               line, expr)
  }
  if (!(type %in% INTERPOLATION_TYPES)) {
    odin_error(sprintf(
      "Invalid interpolation type; must be one: of %s",
      paste(INTERPOLATION_TYPES, collapse = ", ")),
      line, expr)
  }
  if (!is.symbol(m$t)) {
    odin_error("interpolation time argument must be a symbol", line, expr)
  }
  if (!is.symbol(m$y)) {
    odin_error("interpolation target argument must be a symbol", line, expr)
  }
  t <- as.character(m$t)
  y <- as.character(m$y)

  list(interpolate = list(t = t, y = y, type = type),
       depends = ir_parse_depends(variables = c(t, y)))
}


ir_parse_expr_rhs_delay <- function(rhs, line, expr, source) {
  ## TODO: do we get sensible errors here with missing args, extra args, etc?
  ## m <- match.call(function(expr, by, default = NULL) NULL, rhs, FALSE)
  na <- length(rhs) - 1L
  if (na < 2L || na > 3L) {
    ir_odin_error("delay() requires two or three arguments",
                  line, source)
  }

  delay_expr <- rhs[[2L]]
  delay_time <- rhs[[3L]]
  if (na == 3L) {
    stop("checkme")
    delay_default <- ir_parse_expr_rhs(rhs[[4L]], line, expr, source)
  } else {
    delay_default <- NULL
  }

  deps_delay_expr <- find_symbols(delay_expr)
  deps_delay_time <- find_symbols(delay_time)
  fns <- c(deps_delay_expr$functions,
           deps_delay_time$functions,
           delay_default$depends$functions)

  if ("delay" %in% fns) {
    ir_odin_error("delay() may not be nested", line, source)
  }

  if (TIME %in% deps_delay_expr$variables) {
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
    ir_odin_error("delay() may not refer to time as that's confusing",
                  line, source)
  }

  if (is.recursive(delay_time) && !is_call(delay_time, quote(`(`))) {
    ## TODO: I don't think this is needed
    stop("checkme")
    delay_time <- call("(", delay_time)
  }

  depends <- join_deps(list(deps_delay_time, delay_default$depends))

  list(delay = list(time = delay_time,
                    default = delay_default,
                    depends = deps_delay_expr),
       rhs = list(value = delay_expr),
       depends = depends)
}


ir_parse_equations <- function(eqs) {
  type <- vcapply(eqs, "[[", "type")
  eqs <- eqs[!(type %in% c("null", "config"))]
  ## At some point we'll move this around
  f <- function(x) {
    x$lhs <- x$lhs$name_lhs
    x
  }
  eqs
}


ir_parse_depends <- function(functions = character(0),
                             variables = character(0)) {
  if (length(functions) == 0L && length(variables) == 0L) {
    NULL
  } else {
    list(functions = functions, variables = variables)
  }
}


ir_parse_interpolate <- function(eqs, discrete, source) {
  type <- vcapply(eqs, "[[", "type")
  for (eq in eqs[type == "interpolate"]) {
    eqs <- ir_parse_interpolate1(eq, eqs, discrete, source)
  }
  eqs
}


ir_parse_interpolate1 <- function(eq, eqs, discrete, source) {
  nm <- eq$lhs$name_lhs

  nm_alloc <- sprintf("interpolate_%s", nm)
  eq_alloc <- eq
  eq_alloc$name <- nm_alloc
  eq_alloc$type <- "alloc_interpolate"
  eq_alloc$lhs$name_lhs <- nm_alloc
  eq_alloc$lhs$name_data <- nm_alloc
  eq_alloc$lhs$name_equation <- nm_alloc
  eq_alloc$lhs$storage_mode <- "interpolate_data"

  msg <- setdiff(c(eq_alloc$interpolate$t, eq_alloc$interpolate$y), names(eqs))
  if (length(msg) > 0L) {
    ## TODO: duplicates main dependency checking errors
    fmt <-
      ngettext(length(msg), "Unknown variable %s", "Unknown variables %s")
    ir_odin_error(sprintf(fmt, paste(msg, collapse = ", ")),
                  eq$source, source)
  }

  eq_t <- eqs[[eq_alloc$interpolate$t]]
  eq_y <- eqs[[eq_alloc$interpolate$y]]

  rank_t <- eq_t$array$rank
  rank_y <- eq_y$array$rank
  rank_z <- eq$array$rank %||% 0L

  if (eq_t$array$rank != 1L) {
    ## TODO: These error messages should reflect both equations
    ir_odin_error(sprintf("Expected %s to be a vector for interpolation",
                          eq_t$name),
                  eq_t$source, source)
  }

  if (eq_y$array$rank != rank_z + 1L) {
    type <-
      if (rank_z == 0L) "vector" else paste(rank_z + 1, "dimensional array")
    ir_odin_error(sprintf("Expected %s to be a %s", eq_y$name, type),
                  eq_y$source, source)
  }

  eq_alloc$interpolate$equation <- nm
  time <- if (discrete) STEP else TIME

  ## TODO: this is going to become "interpolate", because that is
  ## needed to support C code generation.
  eq_use <- eq
  eq_use$type <- "expression_scalar"
  eq_use$depends <- ir_parse_depends(variables = c(time, nm_alloc))
  eq_use$interpolate <- NULL # becomes `nm_alloc`
  eq_use$rhs <- list(value = call("interpolate", as.name(nm_alloc)))

  ## TODO: this will switch over to be on eq_use once it changes type
  type <- eq_alloc$interpolate$type
  eq_alloc$control <- list(
    min = eq_t$name,
    max = if (type != "constant") eq_t$name,
    critical = if (type == "constant") eq_t$name)

  extra <- list(eq_alloc, eq_use)
  names(extra) <- vcapply(extra, "[[", "name")

  stopifnot(sum(names(eqs) == eq$name) == 1)
  c(eqs[names(eqs) != eq$name], extra)
}


## We're going to need to wrap this up like testthat I think, so that
## we can catch these and group them together.  But leaving that for
## now.
ir_odin_error <- function(msg, line, source) {
  ret <- ir_odin_info_data(msg, line, source, "error")
  class(ret) <- c("odin_error", "error", "condition")
  stop(ret)
}


ir_odin_info_data <- function(msg, line, source, type) {
  expr <- source[line]
  str <- odin_info_expr(line, expr)
  list(message = paste0(msg, paste0("\n\t", str, collapse = "")),
       msg = msg,
       line = line,
       expr = expr,
       type = type)
}


ir_get_lines <- function(eqs) {
  unlist(unname(lapply(eqs, "[[", "source")))
}


## TODO: this would be nice to do via substitutions, but that does
## require getting the order of equations correct, because we need to
## include the variables in the graph (which we don't normally do).
ir_parse_rewrite_initial <- function(eq, variables) {
  needs_rewrite <- identical(eq$lhs$special, "initial") &&
    any(eq$depends$variables %in% variables)
  if (needs_rewrite) {
    subs <- set_names(initial_name(variables), variables)

    env <- as.environment(lapply(subs, as.name))
    eq$rhs$value <- substitute_(eq$rhs$value, env)

    i <- match(eq$depends$variables, names(subs))
    j <- !is.na(i)
    eq$depends$variables[j] <- subs[i][j]
  }

  eq
}


ir_parse_check_functions <- function(eqs, discrete, source) {
  used_functions <- lapply(eqs, function(x) x$depends$functions)
  all_used_functions <- unique(unlist(used_functions))

  if (!discrete) {
    err <- intersect(all_used_functions, names(FUNCTIONS_STOCHASTIC))
    if (length(err) > 0L) {
      tmp <- eqs[vlapply(used_functions, function(x) any(x %in% err))]
      ir_odin_error(sprintf(
        "Stochastic functions not allowed in ODE models (used: %s)",
        pastec(err)),
        ir_get_lines(tmp), source)
    }
  }

  allowed <- c(names(FUNCTIONS),
               names(FUNCTIONS_INFIX),
               names(FUNCTIONS_UNARY),
               names(FUNCTIONS_RENAME),
               if (discrete) names(FUNCTIONS_STOCHASTIC))
  ## TODO:
  ## FUNCTIONS_SUM,
  ## names(obj$config$include$declarations))

  err <- setdiff(all_used_functions, allowed)
  if (length(err) > 0L) {
    tmp <- obj$eqs[vlapply(used_functions, function(x) any(x %in% err))]
    ir_odin_error(sprintf("Unsupported %s: %s",
                       ngettext(length(err), "function", "functions"),
                       pastec(err)),
                  ir_get_lines(tmp), source)
  }
}


## See ir_parse_interpolate for the same pattern
ir_parse_delay <- function(eqs, discrete, variables, source) {
  type <- vcapply(eqs, "[[", "type")
  for (eq in eqs[type == "delay"]) {
    if (discrete) {
      eqs <- ir_parse_delay_discrete(eq, eqs, source)
      initial_time <- initial_name(STEP)
      initial_time_type <- "int"
    } else {
      eqs <- ir_parse_delay_continuous(eq, eqs, variables, source)
      initial_time <- initial_name(TIME)
      initial_time_type <- "double"
    }
  }

  eq_initial_time <- list(list(
    name = initial_time,
    type = "null",
    lhs = list(name_data = initial_time, name_equation = initial_time,
               special = "initial", storage_mode = initial_time_type),
    line = integer(0),
    depends = NULL))
  names(eq_initial_time) <- initial_time

  c(eqs, eq_initial_time)
}


ir_parse_delay_discrete <- function(eq, eqs, source) {
  nm <- eq$name
  nm_ring <- sprintf("delay_ring_%s", nm)

  depends_ring <- list(functions = character(0),
                       variables = eq$array$dimnames$length %||% character(0))
  lhs_ring <- list(name_data = nm_ring, name_equation = nm_ring,
                   name_lhs = nm_ring, storage_mode = "ring_buffer")
  eq_ring <- list(
    name = nm_ring,
    type = "alloc_ring",
    source = eq$source,
    depends = depends_ring,
    lhs = lhs_ring,
    delay = nm)

  lhs_use <- eq$lhs[c("name_data", "name_equation", "name_lhs", "special")]
  depends_use <- join_deps(list(eq$depends, eq$delay$depends))
  depends_use$variables <- c(nm_ring, depends_use$variables)
  eq_use <- list(
    name = nm,
    type = "delay_discrete",
    source = eq$source,
    depends = depends_use,
    lhs = lhs_use,
    rhs = list(value = eq$rhs$value,
               index = eq$rhs$index),
    array = eq$array,
    delay = list(ring = nm_ring,
                 time = eq$delay$time,
                 default = eq$delay$default))

  extra <- list(eq_ring, eq_use)
  names(extra) <- vcapply(extra, "[[", "name")

  stopifnot(sum(names(eqs) == eq$name) == 1)
  c(eqs[names(eqs) != eq$name], extra)
}


ir_parse_delay_continuous <- function(eq, eqs, variables, source) {
  ## So we need to build:
  ## * delay index
  ## * delay state

  nm <- eq$name
  nm_state <- sprintf("delay_state_%s", nm)
  nm_index <- sprintf("delay_index_%s", nm)
  nm_dim <- sprintf("dim_delay_%s", nm)

  graph <- ir_parse_delay_continuous_graph(eq, eqs, variables, source)

  ## TODO: determine if any of the dependent *equations* are arrays
  arrays <- names_if(
    vcapply(eqs[graph$equations], "[[", "type") == "expression_array")
  if (length(arrays) > 0L) {
    stop("write substitiutions")
  } else {
    substitutions <- list()
  }

  eq_len <- list(
    name = nm_dim,
    type = "expression_scalar",
    source = eq$source,
    depends = find_symbols(graph$packing$length),
    lhs = list(name_data = nm_dim, name_equation = nm_dim, name_lhs = nm_dim,
               storage_mode = "int"),
    rhs = list(value = graph$packing$length))

  lhs_use <- eq$lhs[c("name_data", "name_equation", "name_lhs", "special")]
  depends_use <- join_deps(list(eq$depends,
                                ir_parse_depends(variables = nm_dim)))
  eq_use <- list(
    name = nm,
    type = "delay_continuous",
    source = eq$source,
    depends = depends_use,
    lhs = lhs_use,
    rhs = list(value = eq$rhs$value),
    delay = list(
      state = nm_state,
      index = nm_index,
      substitutions = substitutions,
      variables = list(length = eq_len$name,
                       contents = graph$packing$contents),
      equations = graph$equations,
      default = eq$delay$default,
      time = eq$delay$time))

  array <- list(dimnames = list(length = nm_dim, dim = NULL, mult = NULL),
                rank = 1L)
  lhs_index <-
    list(name_data = nm_index, name_equation = nm_index, name_lhs = nm_index,
         storage_mode = "int")
  eq_index <- list(
    name = nm_index,
    type = "delay_index",
    source = eq$source,
    depends = ir_parse_depends(variables = nm_dim),
    lhs = lhs_index,
    delay = nm,
    array = array)

  lhs_state <-
    list(name_data = nm_state, name_equation = nm_state, name_lhs = nm_state,
         storage_mode = "double")
  eq_state <- list(
    name = nm_state,
    type = "null",
    source = eq$source,
    depends = ir_parse_depends(variables = nm_dim),
    lhs = lhs_state,
    array = array)

  extra <- list(eq_len, eq_index, eq_state, eq_use)
  names(extra) <- vcapply(extra, "[[", "name")

  stopifnot(sum(names(eqs) == eq$name) == 1)
  c(eqs[names(eqs) != eq$name], extra)
}


ir_parse_delay_continuous_graph <- function(eq, eqs, variables, source) {
  ## We have to look through all dependencies here.  This duplicates
  ## most of the dependency/stage resolution code elsewhere.  But this
  ## needs to be resolved separately I think.
  used <- eq$delay$depends$variables

  v <- setdiff(used, variables)
  deps <- list()
  exclude <- c(variables, TIME)
  while (length(v) > 0L) {
    if (!all(v %in% names(eqs))) {
      stop("FIXME")
    }
    tmp <- lapply(eqs[v], function(x) x$depends$variables)
    deps <- c(deps, tmp)
    v <- setdiff(unlist(tmp, use.names = FALSE), c(exclude, names(deps)))
  }

  used_vars <- intersect(variables,
                         union(used, unlist(deps, use.names = FALSE)))
  used_eqs <- topological_order(deps) %||% character(0)

  include <- set_names(logical(length(used_eqs)), used_eqs)
  for (v in used_eqs) {
    d <- deps[[v]]
    include[[v]] <-
      any(d %in% used_vars) || any(include[intersect(d, names(deps))])
  }
  used_eqs <- used_eqs[include]

  ## Then we need to compute a packing for each variable, which
  ## duplicates some code that determines the storage types.
  i <- vlapply(eqs, function(x)
    identical(x$lhs$special, "deriv") && x$lhs$name_data %in% used_vars)
  tmp <- unname(eqs[i])
  len <- lapply(tmp, function(x) x$array$dimnames$length %||% 1L)
  rank <- viapply(tmp, function(x) x$array$rank %||% 0L)
  names <- vcapply(tmp, function(x) x$lhs$name_data)
  packing <- ir_parse_packing_internal(names, rank, len)

  list(equations = used_eqs, variables = used_vars, packing = packing)
}
