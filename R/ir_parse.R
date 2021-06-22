ir_parse <- function(x, options, type = NULL) {
  xp <- odin_preprocess(x, type)
  root <- xp$root
  exprs <- xp$exprs
  base <- xp$base

  dat <- ir_parse_exprs(exprs)
  eqs <- dat$eqs
  source <- dat$source

  ## Data elements:
  config <- ir_parse_config(eqs, base, root, source, options$read_include,
                            options$config_custom)
  features <- ir_parse_features(eqs, config, source)

  variables <- ir_parse_find_variables(eqs, features$discrete, source)

  eqs <- lapply(eqs, ir_parse_rewrite_initial, variables)
  eqs <- ir_parse_arrays(eqs, variables, config$include$names, source)

  ## This performs a round of optimisation where we try to simplify
  ## away expressions for the dimensions, which reduces the number of
  ## required variables.
  eqs <- ir_parse_substitute(eqs, options$substitutions)
  if (options$rewrite_constants) {
    eqs <- ir_parse_rewrite_constants(eqs)
  } else if (options$rewrite_dims && features$has_array) {
    eqs <- ir_parse_rewrite_dims(eqs)
  }

  packing <- ir_parse_packing(eqs, variables, source)
  eqs <- c(eqs, packing$offsets)
  packing$offsets <- NULL

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
    eqs <- ir_parse_delay(eqs, features$discrete, packing$variables, source)
  }

  eqs <- eqs[order(names(eqs))]

  meta <- ir_parse_meta(features$discrete)

  ## If we have arrays, then around this point we will also be
  ## generating a number of additional offset and dimension length
  ## variables.  So watch for the "data" element to have extra return
  ## objects perhaps.

  dependencies <- ir_parse_dependencies(eqs, variables, meta$time, source)
  stage <- ir_parse_stage(eqs, dependencies, variables, meta$time, source)

  eqs_initial <- names_if(vlapply(eqs, function(x)
    identical(x$lhs$special, "initial")))
  features$initial_time_dependent <-
    features$has_delay || max(stage[eqs_initial]) == STAGE_TIME

  data <- ir_parse_data(eqs, packing, stage, source)

  if (features$has_user) {
    is_user <- vcapply(eqs, "[[", "type") == "user"
    user <- unname(lapply(eqs[is_user], function(x)
      list(name = x$name, has_default = !is.null(x$user$default))))
    user <- user[order(vlapply(user, "[[", "has_default"))]
  } else {
    user <- list()
  }

  components <- ir_parse_components(eqs, dependencies, variables, stage,
                                    features$discrete, source, options)
  equations <- ir_parse_equations(eqs)

  ## TODO: it's a bit unclear where this best belongs
  ir_parse_check_functions(eqs, features$discrete, config$include$names, source)

  ret <- list(version = .odin$version,
              config = config,
              meta = meta,
              features = features,
              data = data,
              equations = equations,
              components = components,
              user = user,
              interpolate = interpolate,
              source = source)
  ir <- ir_serialise(ret, options$pretty)
  if (options$validate) {
    ir_validate(ir, TRUE)
  }
  ir
}


ir_parse_packing <- function(eqs, variables, source) {
  ## TODO: this does not actually use 'variables' to determine the
  ## initial conditions.
  i <- vlapply(eqs, function(x) identical(x$lhs$special, "initial"))
  pack_variables <- ir_parse_packing_new(eqs[i], TRUE, "variable")

  j <- vlapply(eqs, function(x) identical(x$lhs$special, "output"))
  pack_output <- ir_parse_packing_new(eqs[j], FALSE, "output")

  output <- vcapply(pack_output$contents, "[[", "name")
  err <- intersect(output, variables)

  if (length(err) > 0L) {
    k <- which(i | j)
    k <- k[vlapply(eqs[k], function(x) x$lhs$name_data %in% err)]
    ir_parse_error("output() name cannot be the same as variable name",
                   ir_parse_error_lines(eqs[k]), source)
  }

  list(variables = pack_variables,
       output = pack_output,
       offsets = c(pack_variables$offsets, pack_output$offsets))
}


ir_parse_data <- function(eqs, packing, stage, source) {
  type <- vcapply(eqs, function(x) x$type, USE.NAMES = FALSE)
  is_alloc <- vlapply(eqs, function(x)
    x$type == "alloc" && x$name != x$lhs$name_lhs)
  i <- !(is_alloc | type %in% c("copy", "config"))

  elements <- lapply(eqs[i], ir_parse_data_element, stage)
  names(elements) <- vcapply(elements, "[[", "name")
  ## For ease of comparison:
  elements <- elements[order(names(elements))]

  list(elements = elements,
       variable = packing$variables,
       output = packing$output)
}


ir_parse_data_element <- function(x, stage) {
  name <- x$lhs$name_data

  storage_type <- x$lhs$storage_type %||% "double"
  if (is.null(x$array)) {
    rank <- 0L
    dimnames <- NULL
  } else {
    rank <- x$array$rank
    dimnames <- x$array$dimnames
  }

  stage <- stage[[x$name]]

  if (is.null(x$lhs$special)) {
    if (rank == 0L && stage == STAGE_TIME) {
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
    stop("unclassified data type [odin bug]") # nocov
  }

  list(name = name,
       location = location,
       stage = stage,
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


ir_parse_find_variables <- function(eqs, discrete, source) {
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
    ir_parse_error(sprintf(
      "%s() and initial() must contain same set of equations:\n%s\n",
      rhs_fun, paste(msg$get(), collapse = "\n")),
      ir_parse_error_lines(tmp), source)
  }

  err <- names(eqs) %in% vars
  if (any(err)) {
    ir_parse_error(
      sprintf("variables on lhs must be within %s() or initial() (%s)",
              rhs_fun,
              paste(intersect(vars, names(eqs)), collapse = ", ")),
      ir_parse_error_lines(eqs[err]), source)
  }

  unique(unname(vars))
}


## This is a bit of a faff for now, and finds only *exclusive* output
## variables.  If we drop the "copy" equation type and always work in
## place for output variables this will work.  However, for that to
## work we need special treatment of
##
## > output(x[]) <- x[i]
##
## because in that case an arbitrary transformation is currently
## allowed, for example
##
## > output(x[]) <- x[i] / 2
##
## which is really quite different.
ir_parse_find_exclusive_output <- function(eqs, source) {
  i <- vlapply(eqs, function(x) identical(x$lhs$special, "output"))
  j <- vlapply(eqs, function(x) is.null(x$lhs$special))
  nms <- vcapply(eqs[i], function(x) x$lhs$name_data, USE.NAMES = FALSE)
  setdiff(nms, names_if(j))
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
    ir_parse_error(sprintf(fmt, paste(msg, collapse = ", ")),
                   ir_parse_error_lines(eqs[i]), source)
  }

  order <- topological_order(deps)

  ## Then, we work out the recursive dependencies; this is the entire
  ## dependency chain of a thing; including its dependencies, its
  ## dependencies dependencies and so on.
  recursive_dependencies(order, deps, variables)
}


ir_parse_stage <- function(eqs, dependencies, variables, time_name, source) {
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

  i <- vlapply(eqs, function(x) !is.null(x$array))
  len <- lapply(eqs[i], function(x) x$array$dimnames$length)
  ## We end up with sometimes a string and sometimes a symbol here
  ## which is unsatisfactory.
  len_var <- vcapply(len[!vlapply(len, is.numeric)], as.character)
  err <- stage[len_var] == STAGE_TIME

  if (any(err)) {
    ## TODO: in the case where we rewrite dimensions this error is not
    ## great beause we've lost the dim() call!
    ir_parse_error(
      "Array extent is determined by time",
      ir_parse_error_lines(eqs[len_var[err]]), source)
  }

  stage
}


ir_parse_packing_new <- function(eqs, variables, offset_prefix) {
  eqs <- unname(eqs)
  len <- lapply(eqs, function(x) x$array$dimnames$length %||% 1L)
  rank <- viapply(eqs, function(x) x$array$rank %||% 0L)
  names <- vcapply(eqs, function(x) x$lhs$name_data)
  ir_parse_packing_internal(names, rank, len, variables, offset_prefix)
}


ir_parse_packing_internal <- function(names, rank, len, variables,
                                      offset_prefix) {
  ## We'll pack from least to most complex and everything with a fixed
  ## offset first. This puts all scalars first, then all arrays that
  ## have compile-time size next (in order of rank), then all arrays
  ## with user-time size (in order of rank).
  i <- order(!vlapply(len, is.numeric), rank)
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
    } else {
      len_i <- if (is.numeric(len[[i]])) len[[i]] else as.name(len[[i]])
      offset[[i + 1L]] <- static_eval(call("+", offset[[i]], len_i))
    }
  }

  ## Split those back apart
  length <- offset[[length(names) + 1L]]
  offset <- offset[seq_along(names)]

  ## Create auxillary offset variables
  i <- vlapply(offset[seq_along(names)], is.call)
  if (any(i)) {
    eq_offset <- function(name, value) {
      list(name = name,
           type = "expression_scalar",
           implicit = TRUE,
           source = integer(0),
           depends = find_symbols(value),
           lhs = list(name_data = name, name_equation = name, name_lhs = name,
                      storage_type = "int"),
           rhs = list(value = value))
    }
    offset_name <- sprintf("offset_%s_%s", offset_prefix, names[i])
    eqs_offsets <- Map(eq_offset, offset_name, offset[i])
    names(eqs_offsets) <- vcapply(eqs_offsets, "[[", "name")
    offset[i] <- lapply(names(eqs_offsets), as.name)
  } else {
    eqs_offsets <- NULL
  }

  ## And pack it all up:
  if (variables) {
    contents <- unname(Map(
      list, name = names, offset = offset, initial = initial_name(names)))
  } else {
    contents <- unname(Map(list, name = names, offset = offset))
  }

  list(length = length, contents = contents, offsets = eqs_offsets)
}


## TODO: this part will change.  Things that are set NULL here will be
## checked elsewhere and are set to NULL so that they can't be easily
## used elsewhere, and the IR validation will check that we've added
## them.
##
## A downside of the approach here is that we do make the checks in a
## few different places.  It might be worth trying to shift more of
## this classification into the initial equation parsing.
ir_parse_features <- function(eqs, config, source) {
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
    ir_parse_error("Cannot mix deriv() and update()",
                   ir_parse_error_lines(tmp), source)
  }
  if (!any(is_update | is_deriv)) {
    ir_parse_error("Did not find a deriv() or an update() call",
                   NULL, NULL)
  }

  list(discrete = any(is_update),
       has_array = any(is_dim),
       has_output = any(is_output),
       has_user = any(is_user),
       has_delay = any(is_delay),
       has_interpolate = any(is_interpolate),
       has_stochastic = any(is_stochastic),
       has_include = !is.null(config$include),
       initial_time_dependent = NULL)
}


ir_parse_components <- function(eqs, dependencies, variables, stage,
                                discrete, source, options) {
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

  type <- vcapply(eqs, "[[", "type")
  core <- unique(c(initial, rhs, output, eqs_initial, eqs_rhs, eqs_output))

  used_in_delay <- unlist(lapply(eqs[type == "delay_continuous"], function(x)
    x$delay$depends$variables), FALSE, FALSE)
  core <- union(core, used_in_delay)
  ignore <- c("config", "null", "delay_index", "alloc")
  core <- c(core, names(eqs)[type %in% ignore])

  if (!options$no_check_unused_equations) {
    ir_parse_check_unused(eqs, dependencies, core, stage, source)
  }

  list(
    create = list(variables = character(0), equations = eqs_constant),
    user = list(variables = character(0), equations = eqs_user),
    initial = list(variables = character(0), equations = eqs_initial),
    rhs = list(variables = variables_rhs, equations = eqs_rhs),
    output = list(variables = variables_output, equations = eqs_output))
}


ir_parse_check_unused <- function(eqs, dependencies, core, stage, source) {
  used <- unique(c(core, unlist(dependencies[core], FALSE, FALSE)))
  check <- names_if(vlapply(eqs, function(x) !isTRUE(x$implicit)))
  unused <- setdiff(check, used)

  ## Check to make sure that we don't mark equations as
  ## ignorable. Practically this will just be for user and constant
  ## variables as missing time-varying variables will be excluded
  ## due to not contributing to the graph.
  ignored <- vlapply(eqs[unused], function(x)
    any(grepl("#\\s*ignore\\.unused", source[x$source])),
    USE.NAMES = FALSE)

  ## This is almost certainly not what is wanted, but for now we'll
  ## just raise a message rather than an error:
  if (length(ignored) > 0) {
    dropped <- names_if(stage[unused[ignored]] == STAGE_TIME)
    if (length(dropped) > 0) {
      ir_parse_note(sprintf(
        "Unused equation marked as ignored will be dropped: %s",
        paste(sort(dropped), collapse = ", ")),
        ir_parse_error_lines(eqs[dropped]), source)
    }
  }

  unused <- unused[!ignored]
  if (length(unused) > 0L) {
    ## NOTE: at this point it would be nicest to unravel the
    ## dependency graph a bit to find the variables that are really
    ## never used; these are the ones that that the others come from.
    ## But at this point all of these can be ripped out so we'll just
    ## report them all:
    what <- ngettext(length(unused), "equation", "equations")
    ir_parse_note(sprintf("Unused %s: %s",
                          what, paste(sort(unused), collapse = ", ")),
                  ir_parse_error_lines(eqs[unused]), source)
  }
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

  expr_is_assignment <- function(x) {
    length(x) == 3L &&
      (identical(x[[1]], quote(`<-`)) || identical(x[[1]], quote(`=`)))
  }
  err <- which(!vlapply(exprs, expr_is_assignment))
  if (length(err) > 0L) {
    ir_parse_error("Every line must contain an assignment",
                   unlist(lines[err]), src)
  }

  eqs <- Map(ir_parse_expr, exprs, lines, MoreArgs = list(source = src))
  names(eqs) <- vcapply(eqs, "[[", "name")
  list(eqs = eqs, source = src)
}


ir_parse_expr <- function(expr, line, source) {
  lhs <- ir_parse_expr_lhs(expr[[2L]], line, source)
  rhs <- ir_parse_expr_rhs(expr[[3L]], line, source)
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
    stop("unclassified equation type [odin bug]") # nocov
  }
  lhs$type <- NULL

  ## Below here uses both the lhs and rhs:
  if (type == "user") {
    if (!is.null(lhs$special) && !identical(lhs$special, "dim")) {
      ir_parse_error("user() only valid for non-special variables",
                     line, source)
    }
    if (rhs$user$integer) {
      lhs$storage_type <- "int"
    }
  }

  ## This might actually be too strict because it's possible that dydt
  ## could be delayed dzdt but that seems unlikely.  Definitely cannot
  ## be most of the others.
  if (type == "delay" && !is.null(lhs$special)) {
    ir_parse_error("delay() only valid for non-special variables",
                   line, source)
  }

  if (identical(lhs$special, "output")) {
    copy_expr <- as.name(lhs$name_data)
    if (type == "expression_array") {
      copy_expr_index <- lapply(INDEX[seq_along(lhs$index)[[1]]], as.name)
      copy_expr_array <- as.call(c(as.name("["), copy_expr, copy_expr_index))
    } else {
      copy_expr_array <- copy_expr
    }
    is_copy <- isTRUE(rhs$rhs$value) ||
      identical(rhs$rhs$value, copy_expr) ||
      identical(rhs$rhs$value, copy_expr_array)
    if (is_copy) {
      type <- "copy"
      depends <- list(functions = character(0), variables = lhs$name_data)
      rhs <- NULL
    }
  }

  ## There is heaps of work required here, unfortunately
  if (any(names(FUNCTIONS_INPLACE) %in% depends$functions)) {
    type <- "expression_inplace"
    ## TODO: should check here that the lhs is really complete, but
    ## that requires being able to see empty indices.
    ir_parse_expr_rhs_check_inplace(lhs, rhs, line, source)
  }

  ## NOTE: arrays are the only case where self referential variables
  ## are allowed.  For arrays, there's no checking here and things like
  ## > x[i] = x[i] * 2
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
    ir_parse_error(
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


ir_parse_expr_lhs <- function(lhs, line, source) {
  is_special <- is_array <- FALSE
  special <- index <- depends <- NULL

  if (is.call(lhs)) {
    fun <- deparse_str(lhs[[1L]])
    if (fun %in% SPECIAL_LHS) {
      if (length(lhs) != 2L) {
        ir_parse_error("Invalid length special function on lhs", line, source)
      }
      is_special <- TRUE
      special <- fun
      lhs <- lhs[[2L]]
    }
  }

  if (is_call(lhs, "[")) {
    if (is_special && special %in% c("dim", "config")) {
      ir_parse_error("dim() must be applied to a name only (not an array)",
                     line, source)
    }
    is_array <- TRUE
    tmp <- ir_parse_expr_lhs_index(lhs, line, source)
    index <- tmp$index
    depends <- tmp$depends
    lhs <- lhs[[2L]]
  }

  name <- ir_parse_expr_check_lhs_name(lhs, special, line, source)
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
    stop("odin bug") # nocov
  }

  list(type = type,
       name_data = name_data,
       name_equation = name_equation,
       name_lhs = name_lhs,
       special = special,
       index = index,
       depends = depends)
}


ir_parse_expr_lhs_index <- function(lhs, line, source) {
  if (!is.name(lhs[[2L]])) {
    ir_parse_error("array lhs must be a name", line, source)
  }

  index <- as.list(lhs[-(1:2)]) # nolint

  is_empty <- vlapply(index, identical, quote(expr = )) # nolint
  ## TODO: it might be useful to treat these specially rather than
  ## filling them in like this.
  if (any(is_empty)) {
    if (length(index) == 1L) {
      index[] <- list(bquote(1:length(.(lhs[[2L]])))) # nolint
    } else {
      index[is_empty] <- lapply(as.numeric(which(is_empty)), function(i)
        bquote(1:dim(.(lhs[[2L]]), .(i))))
    }
    lhs[-(1:2)] <- index # nolint
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
  tmp <- lapply(index, ir_parse_expr_lhs_check_index)
  ok <- vlapply(tmp, as.logical)
  if (all(ok)) {
    extent_max <- lapply(tmp, attr, "value_max", exact = TRUE)
    extent_min <- lapply(tmp, attr, "value_min", exact = TRUE)
    is_range <- !vlapply(extent_min, is.null)
  } else {
    msg <- paste0("\t\t", vcapply(tmp[!ok], attr, "message"), collapse = "\n")
    ir_parse_error(sprintf("Invalid array use on lhs:\n%s", msg),
                   line, source)
  }

  name <- deparse(lhs[[2L]])
  deps <- find_symbols(index)
  err <- intersect(INDEX, deps$variables)
  if (length(err) > 0L) {
    ir_parse_error(
      sprintf("Special index variable %s may not be used on array lhs",
              pastec(err)), line, source)
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


ir_parse_expr_check_lhs_name <- function(lhs, special, line, source) {
  ## at this point there are lots of other corner cases; things like
  ## nested special functions.
  if (is.call(lhs)) {
    fun <- deparse_str(lhs[[1L]])
    if (fun %in% SPECIAL_LHS) {
      ## no special warning about f(deriv(deriv(x))) but that's ok
      msg <- sprintf("Invalid nested lhs function usage for %s", fun)
    } else {
      msg <- sprintf("Unhandled expression %s on lhs", fun)
    }
    ir_parse_error(msg, line, source)
  }

  ## things like atomic will raise here: 1 <- 2
  if (!is.name(lhs)) {
    if (is.null(special)) {
      ir_parse_error("Invalid left hand side", line, source)
    } else {
      ir_parse_error(sprintf("Argument to %s must be a symbol", special),
                     line, source)
    }
  }

  name <- deparse(lhs)

  if (name %in% RESERVED) {
    ir_parse_error(sprintf("Reserved name '%s' for lhs", name), line, source)
  }
  re <- sprintf("^(%s)_.*", paste(RESERVED_PREFIX, collapse = "|"))
  if (grepl(re, name)) {
    ir_parse_error(sprintf("Variable name cannot start with '%s_'",
                           sub(re, "\\1", name)),
                   line, source)
  }

  name
}


## TODO: the 'expr' part here needs to come out entirely
ir_parse_expr_rhs <- function(rhs, line, source) {
  if (is_call(rhs, quote(delay))) {
    ir_parse_expr_rhs_delay(rhs, line, source)
  } else if (is_call(rhs, quote(user))) {
    ir_parse_expr_rhs_user(rhs, line, source)
  } else if (is_call(rhs, quote(interpolate))) {
    ir_parse_expr_rhs_interpolate(rhs, line, source)
  } else {
    ir_parse_expr_rhs_expression(rhs, line, source)
  }
}


ir_parse_expr_rhs_expression <- function(rhs, line, source) {
  depends <- find_symbols(rhs)
  err <- intersect(setdiff(SPECIAL_LHS, "dim"), depends$functions)
  if (length(err) > 0L) {
    ir_parse_error(sprintf("Function %s is disallowed on rhs",
                           paste(unique(err), collapse = ", ")), line, source)
  }
  err <- intersect(SPECIAL_RHS, depends$functions)
  if (length(err) > 0L) {
    ir_parse_error(sprintf("%s() must be the only call on the rhs", err[[1]]),
                   line, source)
  }
  err <- intersect(c(SPECIAL_LHS, SPECIAL_RHS), depends$variables)
  if (length(err) > 0L) {
    ## NOTE: in the case where more than one bad name is used on the
    ## rhs, we could warn about more of them, but this is already
    ## pretty niche.
    ##
    ## NOTE: could expand this to all allowed functions?
    ir_parse_error(sprintf(
      "Function '%s' is disallowed as symbol on rhs", err[[1L]]),
      line, source)
  }

  ## TODO: look at this later, but it's called only for throwing as
  ## side effect for now, so we can clean it up later easily
  ir_parse_expr_rhs_check_usage(rhs, line, source)

  if ("sum" %in% depends$functions) {
    rhs <- ir_parse_expr_rhs_expression_sum(rhs, line, source)
    depends <- find_symbols(rhs)
  }

  if (":" %in% depends$functions) {
    ir_parse_error("Range operator ':' may not be used on rhs", line, source)
  }

  stochastic <- any(depends$functions %in% names(FUNCTIONS_STOCHASTIC))

  list(rhs = list(value = rhs),
       depends = depends,
       stochastic = stochastic)
}


ir_parse_expr_rhs_user <- function(rhs, line, source) {
  args <- as.list(rhs[-1L])

  nms <- names(args) %||% rep("", length(args))
  if (any(!nzchar(nms[-1]))) {
    ir_parse_error("Only first argument to user() may be unnamed", line, source)
  }

  m <- match.call(function(default, integer, min, max, ...) NULL, rhs, FALSE)
  extra <- m[["..."]]
  if (!is.null(extra)) {
    ir_parse_error(sprintf("Unknown %s to user(): %s",
                           ngettext(length(extra), "argument", "arguments"),
                           paste(squote(names(extra)), collapse = ", ")),
                   line, source)
  }

  ## This looks through default, integer, min, max
  deps <- find_symbols(as.list(rhs[-1L]))
  ## TODO: This could be relaxed I think, but dealing with
  ## potential cycles is hard because they could be generated at
  ## runtime.  So for now, these values must be constants.  I
  ## don't want to relax that until it's clear enough how arrays
  ## get treated here.
  allowed <- c("+", "/", "-", "*", "^", "(")
  if (length(setdiff(deps$functions, allowed)) > 0L) {
    ir_parse_error("user() call must not use functions", line, source)
  }
  if (length(deps$variables) > 0L) {
    ir_parse_error("user() call must not reference variables", line, source)
  }
  ## TODO: the 'dim' part here is not actually known yet!
  user <- list(default = m$default,
               dim = FALSE,
               integer = m$integer %||% FALSE,
               min = m$min,
               max = m$max)
  list(user = user)
}


ir_parse_expr_rhs_interpolate <- function(rhs, line, source) {
  na <- length(rhs) - 1L
  if (na < 2L || na > 3L) {
    ir_parse_error("interpolate() requires two or three arguments",
                   line, source)
  }

  m <- match.call(function(t, y, type) NULL, rhs, FALSE)

  type <- m$type %||% "spline"
  if (!is.character(type)) {
    ir_parse_error("Expected a string constant for interpolation type",
                   line, source)
  }
  if (!(type %in% INTERPOLATION_TYPES)) {
    ir_parse_error(sprintf(
      "Invalid interpolation type; must be one: of %s",
      paste(INTERPOLATION_TYPES, collapse = ", ")),
      line, source)
  }
  if (!is.symbol(m$t)) {
    ir_parse_error("interpolation time argument must be a symbol",
                   line, source)
  }
  if (!is.symbol(m$y)) {
    ir_parse_error("interpolation target argument must be a symbol",
                   line, source)
  }
  t <- as.character(m$t)
  y <- as.character(m$y)

  list(interpolate = list(t = t, y = y, type = type),
       depends = ir_parse_depends(variables = c(t, y)))
}


ir_parse_expr_rhs_delay <- function(rhs, line, source) {
  na <- length(rhs) - 1L
  if (na < 2L || na > 3L) {
    ir_parse_error("delay() requires two or three arguments",
                   line, source)
  }

  delay_expr <- rhs[[2L]]
  delay_time <- rhs[[3L]]
  if (na == 3L) {
    delay_default <- ir_parse_expr_rhs(rhs[[4L]], line, source)
  } else {
    delay_default <- NULL
  }

  deps_delay_expr <- find_symbols(delay_expr)
  deps_delay_time <- find_symbols(delay_time)
  fns <- c(deps_delay_expr$functions,
           deps_delay_time$functions,
           delay_default$depends$functions)

  if ("delay" %in% fns) {
    ir_parse_error("delay() may not be nested", line, source)
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
    ir_parse_error("delay() may not refer to time as that's confusing",
                   line, source)
  }

  depends <- join_deps(list(deps_delay_time, delay_default$depends))

  list(delay = list(time = delay_time,
                    default = delay_default$rhs$value,
                    depends = deps_delay_expr),
       rhs = list(value = delay_expr),
       depends = depends)
}


ir_parse_equations <- function(eqs) {
  type <- vcapply(eqs, "[[", "type")
  eqs[!(type %in% c("null", "config"))]
}


ir_parse_depends <- function(functions = character(0),
                             variables = character(0)) {
  list(functions = functions, variables = variables)
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
  eq_alloc$lhs$storage_type <- "interpolate_data"

  msg <- setdiff(c(eq_alloc$interpolate$t, eq_alloc$interpolate$y), names(eqs))
  if (length(msg) > 0L) {
    ## TODO: duplicates main dependency checking errors
    fmt <-
      ngettext(length(msg), "Unknown variable %s", "Unknown variables %s")
    ir_parse_error(sprintf(fmt, paste(msg, collapse = ", ")),
                   eq$source, source)
  }

  eq_t <- eqs[[eq_alloc$interpolate$t]]
  eq_y <- eqs[[eq_alloc$interpolate$y]]

  rank_t <- eq_t$array$rank
  rank_y <- eq_y$array$rank
  rank_z <- eq$array$rank %||% 0L

  if (eq_t$array$rank != 1L) {
    ## TODO: These error messages should reflect both equations
    ir_parse_error(sprintf("Expected %s to be a vector for interpolation",
                           eq_t$name),
                   eq_t$source, source)
  }

  if (eq_y$array$rank != rank_z + 1L) {
    type <-
      if (rank_z == 0L) "vector" else paste(rank_z + 1, "dimensional array")
    ir_parse_error(sprintf("Expected %s to be a %s", eq_y$name, type),
                   eq_y$source, source)
  }

  eq_alloc$interpolate$equation <- nm
  time <- if (discrete) STEP else TIME

  eq_use <- eq
  eq_use$type <- "interpolate"
  eq_use$depends <- ir_parse_depends(variables = c(time, nm_alloc))
  eq_use$interpolate <- nm_alloc

  ## TODO: this will switch over to be on eq_use once it changes type
  type <- eq_alloc$interpolate$type
  eq_alloc$control <- list(
    min = eq_t$name,
    max = if (type != "constant") eq_t$name,
    critical = if (type == "constant") eq_t$name)

  if (isTRUE(eq$user$dim)) {
    deps_alloc <- union(eq_alloc$depends$variables, eq$array$dimnames$length)
    eq_alloc$depends <- ir_parse_depends(variables = deps_alloc)

    deps <- c(eq$array$dimnames$length, eq$interpolate$t, eq$interpolate$y)
    eq$depends <- ir_parse_depends(variables = deps)

    nm_length <- eq$array$dimnames$length
    eqs[[nm_length]]$type <- "expression_scalar"

    if (rank_z == 1L) {
      len <- eq_y$array$dimnames$dim[[2]]
      eqs[[nm_length]]$rhs$value <- as.name(len)
      eqs[[nm_length]]$depends <- ir_parse_depends(variables = len)
    } else {
      for (j in seq_along(eq$array$dimnames$dim)) {
        nm <- eq$array$dimnames$dim[[j]]
        eqs[[nm]]$type <- "expression_scalar"
        eqs[[nm]]$rhs$value <- as.name(eq_y$array$dimnames$dim[[j + 1]])
        eqs[[nm]]$depends$variables <- eq_y$array$dimnames$dim[[j + 1]]
      }
    }
  }

  extra <- list(eq_alloc, eq_use)
  names(extra) <- vcapply(extra, "[[", "name")

  stopifnot(sum(names(eqs) == eq$name) == 1)
  c(eqs[names(eqs) != eq$name], extra)
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


ir_parse_check_functions <- function(eqs, discrete, include, source) {
  used_functions <- lapply(eqs, function(x) x$depends$functions)
  all_used_functions <- unique(unlist(used_functions))

  if (!discrete) {
    err <- intersect(all_used_functions, names(FUNCTIONS_STOCHASTIC))
    if (length(err) > 0L) {
      tmp <- eqs[vlapply(used_functions, function(x) any(x %in% err))]
      ir_parse_error(sprintf(
        "Stochastic functions not allowed in ODE models (used: %s)",
        pastec(err)),
        ir_parse_error_lines(tmp), source)
    }
  }

  allowed <- c(names(FUNCTIONS),
               names(FUNCTIONS_INFIX),
               names(FUNCTIONS_UNARY),
               names(FUNCTIONS_RENAME),
               "odin_sum",
               include,
               if (discrete) names(FUNCTIONS_STOCHASTIC))

  err <- setdiff(all_used_functions, allowed)
  if (length(err) > 0L) {
    tmp <- eqs[vlapply(used_functions, function(x) any(x %in% err))]
    ir_parse_error(sprintf("Unsupported %s: %s",
                           ngettext(length(err), "function", "functions"),
                           pastec(err)),
                   ir_parse_error_lines(tmp), source)
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

      subs <- unique(unlist(lapply(eqs[names_if(type == "delay")], function(x)
        x$delay$substitutions), FALSE, FALSE))

      ## TODO: ideally we'd get the correct lines here for source, but
      ## that's low down the list of needs.
      f <- function(x) {
        depends <- if (is.numeric(x$dim)) character(0) else as.character(x$dim)
        list(name = x$to,
             type = "alloc",
             source = integer(0),
             depends = ir_parse_depends(variables = depends),
             lhs = list(name_data = x$to,
                        name_lhs = x$to,
                        name_equation = x$to),
             array = eqs[[x$from]]$array)
      }
      arrays <- lapply(subs, f)
      names(arrays) <- vcapply(arrays, "[[", "name")
      eqs <- c(eqs, arrays[setdiff(names(arrays), names(eqs))])
    }
  }

  eq_initial_time <- list(list(
    name = initial_time,
    type = "null",
    lhs = list(name_data = initial_time, name_equation = initial_time,
               special = "initial", storage_type = initial_time_type),
    line = integer(0),
    depends = NULL))
  names(eq_initial_time) <- initial_time

  c(eqs, eq_initial_time)
}


ir_parse_delay_discrete <- function(eq, eqs, source) {
  nm <- eq$name
  nm_ring <- sprintf("delay_ring_%s", nm)

  len <- eq$array$dimnames$length
  depends_ring <- list(
    functions = character(0),
    variables = if (is.character(len)) len else character(0))

  lhs_ring <- list(name_data = nm_ring, name_equation = nm_ring,
                   name_lhs = nm_ring, storage_type = "ring_buffer")
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
  variable_names <- vcapply(variables$contents, "[[", "name")

  nm <- eq$name
  nm_state <- sprintf("delay_state_%s", nm)
  nm_index <- sprintf("delay_index_%s", nm)
  nm_dim <- sprintf("dim_delay_%s", nm)

  graph <- ir_parse_delay_continuous_graph(eq, eqs, variable_names, source)

  arrays <- names_if(
    vcapply(eqs[graph$equations], "[[", "type") == "expression_array")
  if (length(arrays) > 0L) {
    substitutions <- lapply(arrays, function(x)
      list(from = x,
           to = sprintf("delay_array_%s", x),
           dim = eqs[[x]]$array$dimnames$length))
  } else {
    substitutions <- list()
  }

  if (is.numeric(graph$packing$length)) {
    eq_len <- NULL
    val_len <- graph$packing$length
    dep_len <- character(0)
  } else {
    eq_len <- list(
      name = nm_dim,
      type = "expression_scalar",
      source = eq$source,
      depends = find_symbols(graph$packing$length),
      lhs = list(name_data = nm_dim, name_equation = nm_dim, name_lhs = nm_dim,
                 storage_type = "int"),
      rhs = list(value = graph$packing$length))
    val_len <- nm_dim
    dep_len <- nm_dim
  }

  lhs_use <- eq$lhs[c("name_data", "name_equation", "name_lhs", "special")]
  subs_from <- vcapply(substitutions, "[[", "to")
  depends_use <- join_deps(list(
    eq$depends, ir_parse_depends(variables = c(dep_len, subs_from, TIME))))

  eq_use <- list(
    name = nm,
    type = "delay_continuous",
    source = eq$source,
    depends = depends_use,
    lhs = lhs_use,
    rhs = list(value = eq$rhs$value, index = eq$rhs$index),
    delay = list(
      state = nm_state,
      index = nm_index,
      substitutions = substitutions,
      variables = list(length = val_len,
                       contents = graph$packing$contents),
      equations = graph$equations,
      default = eq$delay$default,
      time = eq$delay$time,
      depends = eq$delay$depends),
    array = eq$array)

  array <- list(dimnames = list(length = val_len, dim = NULL, mult = NULL),
                rank = 1L)
  lhs_index <-
    list(name_data = nm_index, name_equation = nm_index, name_lhs = nm_index,
         storage_type = "int")

  ##
  offsets <- lapply(variables$contents[match(graph$variables, variable_names)],
                    "[[", "offset")
  depends_index <- join_deps(lapply(offsets, find_symbols))
  depends_index$variables <- union(depends_index$variables, dep_len)
  eq_index <- list(
    name = nm_index,
    type = "delay_index",
    source = eq$source,
    depends = depends_index,
    lhs = lhs_index,
    delay = nm,
    array = array)

  lhs_state <-
    list(name_data = nm_state, name_equation = nm_state, name_lhs = nm_state,
         storage_type = "double")
  eq_state <- list(
    name = nm_state,
    type = "null",
    source = eq$source,
    depends = ir_parse_depends(variables = dep_len),
    lhs = lhs_state,
    array = array)

  offsets <- graph$packing$offsets
  if (!is.null(offsets)) {
    eq_index$depends$variables <- c(eq_index$depends$variables, names(offsets))
  }

  extra <- c(if (is.null(eq_len)) NULL else list(eq_len),
             list(eq_index, eq_state, eq_use),
             offsets)
  names(extra) <- vcapply(extra, "[[", "name")

  stopifnot(sum(names(eqs) == eq$name) == 1)
  c(eqs[names(eqs) != eq$name], extra)
}


ir_parse_delay_continuous_graph <- function(eq, eqs, variables, source) {
  ## We have to look through all dependencies here.  This duplicates
  ## most of the dependency/stage resolution code elsewhere.  But this
  ## needs to be resolved separately I think.
  used <- eq$delay$depends$variables

  exclude <- c(variables, TIME, INDEX)
  v <- setdiff(used, exclude)
  deps <- list()
  while (length(v) > 0L) {
    err <- setdiff(v, names(eqs))
    if (length(err) > 0L) {
      pos <-
        intersect(union(names(deps), unlist(deps, FALSE, FALSE)), names(eqs))
      msg <- sprintf("Missing %s in delay expression: %s (for delay %s)",
                     ngettext(length(err), "variable", "variables"),
                     paste(err, collapse = ", "), eq$name)
      ir_parse_error(msg, ir_parse_error_lines(eqs[union(pos, eq$name)]),
                     source)
    }
    tmp <- lapply(eqs[v], function(x) x$depends$variables)
    deps <- c(deps, tmp)
    v <- setdiff(unlist(tmp, use.names = FALSE), c(exclude, names(deps)))
  }

  used_vars <- intersect(variables,
                         union(used, unlist(deps, use.names = FALSE)))
  used_eqs <- topological_order(deps) %||% character(0)

  ## Duplicate some of the stage logic here to determine things that
  ## are time dependent.
  include <- set_names(logical(length(used_eqs)), used_eqs)

  ## NOTE: there is a special case here for delay equations because we
  ## are still in the middle of processing them so their
  ## time-dependence is not totally obvious.  That can't easily be
  ## done in the initial parse because of the time/step name split.
  ##
  ## NOTE: the intersect below ejects all orhapsn dependencies (e.g.,
  ## indices)
  for (v in used_eqs) {
    d <- deps[[v]]
    include[[v]] <-
      any(d %in% used_vars) ||
      any(d == TIME) ||
      eqs[[v]]$type == "delay" ||
      any(include[intersect(d, names(deps))])
  }

  used_eqs <- used_eqs[include]

  ## Then we need to compute a packing for each variable, which
  ## duplicates some code that determines the storage types.
  i <- vlapply(eqs, function(x)
    identical(x$lhs$special, "deriv") && x$lhs$name_data %in% used_vars)

  ## TODO: this could get a slightly better offset name - elsewhere we
  ## use "delay_state_<x>", and this is the offset to that, so that
  ## could be "offset_delay_state_<x>_<y>" but we here go with
  ## "offset_<x>_<y>" which is probably long enough really.
  packing <- ir_parse_packing_new(eqs[i], FALSE, eq$name)

  list(equations = used_eqs, variables = used_vars, packing = packing)
}


ir_parse_expr_rhs_check_usage <- function(rhs, line, source) {
  len <- c(FUNCTIONS,
           setNames(FUNCTIONS[FUNCTIONS_RENAME], names(FUNCTIONS_RENAME)))

  throw <- function(...) {
    ir_parse_error(sprintf(...), line, source)
  }

  check_usage <- function(x) {
    if (is.recursive(x)) {
      fn <- x[[1L]]
      if (!is.name(fn)) {
        throw("Cannot process statement")
      }
      nm <- deparse(fn)

      n <- len[[nm]]
      nargs <- length(x) - 1L

      if (nm == "function") {
        throw("Cannot define R functions in odin model")
      }

      if (length(n) > 1L) {
        if (nargs < n[[1L]] || nargs > n[[2L]]) {
          if (is.finite(n[[2L]])) {
            throw("Expected %d-%d arguments in %s call, but recieved %d",
                  n[[1L]], n[[2L]], nm, nargs)
          } else {
            throw("Expected %d or more arguments in %s call, but recieved %d",
                  n[[1L]], nm, nargs)
          }
        }
      } else if (!is.null(n) && is.finite(n)) {
        if (nargs != n) {
          if (nm == "if") {
            ## NOTE: slightly different wording here to make the
            ## problem a little clearer.
            throw("All if statements must have an else clause")
          } else {
            throw("Expected %d %s in %s call, but recieved %d",
                  n, ngettext(n, "argument", "arguments"), nm, nargs)
          }
        }
      }
      lapply(as.list(x[-1L]), check_usage)
    }
  }
  check_usage(rhs)
}


ir_parse_expr_rhs_check_inplace <- function(lhs, rhs, line, source) {
  fn <- deparse(rhs$rhs$value[[1]])
  depends <- join_deps(lapply(rhs$rhs$value[-1], find_symbols))

  ## Start strict, liberalise later
  if (!(fn %in% names(FUNCTIONS_INPLACE)) || length(depends$functions) > 0L) {
    ir_parse_error(sprintf(
      "At present, inplace function '%s' must use no functions", fn),
      line, source)
  }
  if (is.null(lhs$index)) {
    ir_parse_error(sprintf(
      "Expected an array on the lhs of inplace function '%s'", fn),
      line, source)
  }
}


ir_parse_substitute <- function(eqs, subs) {
  if (is.null(subs)) {
    return(eqs)
  }

  f <- function(nm) {
    eq <- eqs[[nm]]
    if (is.null(eq)) {
      stop(sprintf("Substitution failed: '%s' is not an equation", nm),
           call. = FALSE)
    }
    if (eq$type != "user") {
      stop(sprintf("Substitution failed: '%s' is not a user() equation", nm),
           call. = FALSE)
    }
    if (!is.null(eq$array)) {
      stop(sprintf("Substitution failed: '%s' is an array", nm), call. = FALSE)
    }
    value <- support_coerce_mode(subs[[nm]], eq$user$integer,
                                 eq$user$min, eq$user$max, nm)

    eq$type <- "expression_scalar"
    eq$rhs <- list(value = value)
    eq$stochastic <- FALSE
    eq
  }

  eqs[names(subs)] <- lapply(names(subs), f)
  eqs
}


ir_parse_rewrite_dims <- function(eqs) {
  nms <- names_if(vlapply(eqs, function(x) isTRUE(x$lhs$dim)))
  ir_parse_rewrite(nms, eqs)
}


ir_parse_rewrite_constants <- function(eqs) {
  nms <- names_if(vlapply(eqs, function(x) x$type == "expression_scalar"))
  ir_parse_rewrite(nms, eqs)
}


ir_parse_rewrite_compute_eqs <- function(nms, eqs) {
  cache <- new_empty_env()
  lapply(eqs[nms], function(eq)
    static_eval(ir_parse_rewrite_compute(eq$rhs$value, eqs, cache)))
}


ir_parse_rewrite_compute <- function(x, eqs, cache) {
  key <- deparse_str(x)
  if (key %in% names(cache)) {
    return(cache[[key]])
  }

  if (!is.numeric(x)) {
    if (is.symbol(x)) {
      x_eq <- eqs[[deparse_str(x)]]
      ## use identical() here to cope with x_eq being NULL when 't' is
      ## passed through (that will be an error elsewhere).
      if (identical(x_eq$type, "expression_scalar")) {
        x <- ir_parse_rewrite_compute(x_eq$rhs$value, eqs, cache)
      }
    } else if (is_call(x, "length")) {
      ## NOTE: use array_dim_name because we might hit things like
      ## length(y) where 'y' is one of the variables; we can't look up
      ## eqs[[name]]$array$length without checking that.
      length_name <- as.name(array_dim_name(as.character(x[[2]])))
      x <- ir_parse_rewrite_compute(length_name, eqs, cache)
    } else if (is_call(x, "dim")) {
      dim_name <- as.name(array_dim_name(as.character(x[[2]]), x[[3]]))
      x <- ir_parse_rewrite_compute(dim_name, eqs, cache)
    } else if (is.recursive(x)) {
      x[-1] <- lapply(x[-1], ir_parse_rewrite_compute, eqs, cache)
    }
  }

  cache[[key]] <- x
  x
}


ir_parse_rewrite <- function(nms, eqs) {
  val <- tryCatch(
    ir_parse_rewrite_compute_eqs(nms, eqs),
    error = function(e) {
      message("Rewrite failure: ", e$message)
      list()
    })

  rewrite <- vlapply(val, function(x) is.symbol(x) || is.numeric(x))

  subs <- val[rewrite]

  ## One small wrinkle here: don't rewrite things that are the target
  ## of a copy as the rewrite is complicated. This affects almost
  ## nothing in reality outside the tests?
  copy_self <- unlist(lapply(eqs, function(x)
    if (x$type == "copy") x$lhs$name_data), FALSE)
  subs <- subs[setdiff(names(subs), copy_self)]

  is_dim <- vlapply(eqs, function(x) isTRUE(x$lhs$dim))

  ## Try and deduplicate the rest. However, it's not totally obvious
  ## that we can do this without creating some weird dependency
  ## issues. Also we need to only treat dimensions (and possibly
  ## offsets); we could do any compile-time thing really but we don't
  ## know it yet. Propagating other expressions through though can
  ## create problems.
  check <- val[intersect(names_if(!rewrite), names_if(is_dim))]

  if (length(check) > 0) {
    dup <- duplicated(check) & !vlapply(check, is.null)
    if (any(dup)) {
      i <- match(check[dup], check)
      subs <- c(subs,
                set_names(lapply(names(check)[i], as.name), names(check)[dup]))
    }
  }

  subs_env <- list2env(subs, parent = emptyenv())
  subs_dep <- vcapply(subs, function(x)
    if (is.numeric(x)) NA_character_ else deparse_str(x))

  replace <- function(x, y) {
    i <- match(vcapply(x, function(x) x %||% ""), names(y))
    j <- which(!is.na(i))
    x[j] <- unname(y)[i[j]]
    na_drop(x)
  }

  rewrite_eq_array_part <- function(el) {
    el$value <- substitute_(el$value, subs_env)
    for (i in seq_along(el$index)) {
      el$index[[i]]$value <- substitute_(el$index[[i]]$value, subs_env)
    }
    el
  }

  rewrite_eq <- function(eq) {
    if (eq$type == "expression_array") {
      eq$rhs <- lapply(eq$rhs, rewrite_eq_array_part)
    } else if (eq$name %in% names(subs)) {
      eq$rhs$value <- subs[[eq$name]]
    } else {
      eq$rhs$value <- substitute_(eq$rhs$value, subs_env)
    }

    eq$depends$variables <- replace(eq$depends$variables, subs_dep)
    eq$lhs$depends$variables <- replace(eq$lhs$depends$variables, subs_dep)

    if (!is.null(eq$array$dimnames)) {
      eq$array$dimnames$length <- replace(eq$array$dimnames$length, subs)[[1]]
      eq$array$dimnames$dim <- replace(eq$array$dimnames$dim, subs)
      eq$array$dimnames$mult <- replace(eq$array$dimnames$mult, subs)
    }

    if (!is.null(eq$delay)) {
      eq$delay$time <- substitute_(eq$delay$time, subs_env)
      eq$delay$depends$variables <-
        replace(eq$delay$depends$variables, subs_dep)
    }

    eq
  }

  ## Can't drop initial(), deriv(), or update() calls even if they are
  ## constants.
  keep <- names_if(!vlapply(eqs, function(x) is.null(x$lhs$special)))
  i <- setdiff(names(eqs), setdiff(names(subs), keep))
  lapply(eqs[i], rewrite_eq)
}
