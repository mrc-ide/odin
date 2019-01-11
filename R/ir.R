odin_build_ir <- function(x, type = NULL, validate = FALSE, pretty = TRUE) {
  xp <- odin_preprocess(x)
  dat <- ir_prep(odin_parse(xp))

  ## The core functions that we're looking to create are:
  ##
  ##   - initial
  ##   - deriv (or rhs, more generally)
  ##   - output
  ##
  ## for each of these we should declare what we need by recursing
  ## through the dependency tree.  This belongs in the ir and can be
  ## represented simply as a set of arrays
  ir_dat <- list(config = ir_config(dat),
                 features = ir_features(dat),
                 data = ir_data(dat),
                 equations = ir_equations(dat),
                 components = ir_components(dat))
  ir <- ir_serialise(ir_dat, pretty)
  if (validate) {
    ir_validate(ir, TRUE)
  }
  ir
}


## This is going to do some of the prep work that should have been
## done in the parse and which will move there once this settles down
## (in fact, once the ir version is working, we'll refactor the parse
## code entirely to more directly produce the ir).
ir_prep <- function(dat) {
  location <- set_names(rep("internal", nrow(dat$traits)),
                        rownames(dat$traits))
  location[dat$traits[, "is_deriv"]] <- "variable"
  location[dat$traits[, "is_output"]] <- "output"

  ## initial: this is going to be initial conditions and their
  ## non-constant dependencies
  vars <- dat$variable_info$order

  if (dat$info$has_interpolate) {
    i <- dat$traits[, "uses_interpolate"]
    tmp <- lapply(dat$eqs[i], ir_prep_interpolate, dat)
    ## Now we need to patch this into the data.  This is not much fun
    ## because we change orders of things!

    ## TODO: when the parse code is refactored, look carefully at this
    ## as the allocation and the use go into such different parts of
    ## the graph that the current approach is pretty annoying.
    ## However, where the original bits are put is just fine for now.
    ## We'll organise the allocations into the right place too.
    ##
    ## Interpolation is a bit weird because the allocation never ends
    ## up in a dependency and is always going to be done in the user
    ## stage at the end.  So the *use* ends up as a dependency in
    ## others but has no actual dependencies (save time) and the
    ## allocation depends on others.  As such the ordering is always
    ## safe to be: allocation as the first element before time, use as
    ## the first element after time.
    dat$eqs[i] <- lapply(tmp, "[[", "use")
    alloc <- set_names(lapply(tmp, "[[", "alloc"),
                       vcapply(tmp, function(x) x$alloc$name))
    dat$eqs <- c(dat$eqs, alloc)
    dat$stage <- c(dat$stage, viapply(alloc, "[[", "stage"))
    location <- c(location, set_names(rep("internal", sum(i)), names(alloc)))
  }

  ## rhs:
  ## * all equations with a time step?
  ## * all time-dependent dependencies of derivatives? [using this one]
  v <- names_if(dat$traits[, "is_deriv"])
  v_dep <- unique(unlist(dat$deps_rec[v], use.names = FALSE))
  eq_eval_rhs <- set_names(
    names(dat$eqs) %in% c(v, v_dep[dat$stage[v_dep] == STAGE_TIME]),
    names(dat$eqs))
  var_used_rhs <- set_names(vars %in% v_dep, vars)

  ## output:
  v <- names_if(dat$traits[, "is_output"])
  v_dep <- unique(unlist(dat$deps_rec[v], use.names = FALSE))
  eq_eval_output <- set_names(
    names(dat$eqs) %in% c(v, v_dep[dat$stage[v_dep] == STAGE_TIME]),
    names(dat$eqs))
  var_used_output <- set_names(vars %in% v_dep, vars)

  ## create:
  ## * all constant variables
  eq_eval_create <- dat$stage[names(dat$eqs)] == STAGE_CONSTANT

  ## initial:
  v <- names_if(dat$traits[, "is_initial"])
  v_dep <- unique(c(v, unlist(dat$deps_rec[v], use.names = FALSE)))
  eq_eval_initial <- set_names(
    names(dat$eqs) %in% v_dep[dat$stage[v_dep] == STAGE_TIME],
    names(dat$eqs))

  ## user:
  v <- names_if(dat$stage[names(dat$eqs)] == STAGE_USER)
  eq_eval_user <- set_names(names(dat$eqs) %in% v, names(dat$eqs))

  for (i in seq_along(dat$eqs)) {
    dat$eqs[[i]]$lhs$location <- location[[dat$eqs[[i]]$name]]
    dat$eqs[[i]]$deps_rec <- dat$deps_rec[[i]] %||% stop("IR ERROR")
  }

  dat$evaluated <- list(rhs = names_if(eq_eval_rhs),
                        output = names_if(eq_eval_output),
                        create = names_if(eq_eval_create),
                        user = names_if(eq_eval_user),
                        initial = names_if(eq_eval_initial))
  dat$variable_info$used <- list(rhs = names_if(var_used_rhs),
                                 output = names_if(var_used_output))

  dat
}


ir_prep_interpolate <- function(x, dat) {
  stage_alloc <- max(dat$stage[x$depends$variables])
  stopifnot(stage_alloc <= STAGE_USER)
  x_alloc <- x
  x_alloc$name <- x$rhs$value$name
  x_alloc$lhs$name <- x$rhs$value$name
  x_alloc$stage <- stage_alloc
  x_alloc$lhs$data_type <- "interpolate_data" # (really void*)
  x_alloc$rhs$type <- "interpolate_alloc"
  x_alloc$rhs$value <-
    call("interpolate_alloc", x$rhs$value$t, x$rhs$value$y, x$rhs$value$type)

  time <- if (dat$info$discrete) STEP else TIME
  x_use <- x
  x_use$rhs <- list(type = "interpolate_use",
                    value = call(x$rhs$value$name, as.name(time)),
                    interpolate = TRUE)
  x_use$depends <- list(functions = character(),
                        variables = c(time, x$rhs$value$name))

  list(alloc = x_alloc, use = x_use)
}


ir_config <- function(dat) {
  ## We should probably add here:
  ##
  ## - odin version
  ## - file
  ## - type
  ## - path
  ## - include?
  list(base = jsonlite::unbox(dat$config$base))
}


ir_features <- function(dat) {
  v <- c("discrete", "has_array", "has_output", "has_user", "has_delay",
         "has_interpolate", "has_stochastic")
  ## if (dat$info$has_array && dat$info$has_user) {
  ##   ## This is harder because it breaks the ordering code
  ##   stop("check for and enforce user sized arrays")
  ## }
  lapply(dat$info[v], jsonlite::unbox)
}


ir_equations <- function(dat) {
  unname(lapply(dat$eqs, ir_equation))
}


ir_components <- function(dat) {
  v <- c("create", "user", "initial", "rhs", "output")
  f <- function(i) {
    list(variables = dat$variable_info$used[[i]] %||% character(0),
         equations = dat$evaluated[[i]] %||% character(0))
  }
  set_names(lapply(v, f), v)
}


ir_equation <- function(eq) {
  lhs <- list(location = jsonlite::unbox(eq$lhs$location))
  if (is.null(eq$lhs$special)) {
    lhs$target <- jsonlite::unbox(eq$name)
  } else {
    lhs$special <- jsonlite::unbox(eq$lhs$special)
    lhs$target <- jsonlite::unbox(eq$lhs$name_target)
  }

  ## This is the major classification of types: things really route
  ## through different routes later based on this.  Later on we'll
  ## push some of the logic here into the parse functions I think
  ##
  ## TODO: push this into the prep stage?
  if (identical(eq$lhs$special, "dim")) {
    type <- "dim"
  } else if (isTRUE(eq$rhs$user)) {
    type <- "user"
  } else if (isTRUE(eq$rhs$interpolate)) {
    type <- "interpolate"
  } else if (isTRUE(eq$rhs$delay)) {
    type <- "delay"
  } else if (identical(eq$lhs$type, "symbol")) {
    type <- "scalar_expression"
  } else if (identical(eq$lhs$type, "array")) {
    type <- "array_expression"
  } else {
    stop("Unclassified type")
  }

  if (type == "scalar_expression") {
    rhs <- list(
      type = jsonlite::unbox(eq$rhs$type),
      value = ir_expression(eq$rhs$value),
      ## TODO: this conditional would be better in the parse?
      depends = if (eq$rhs$type == "atomic") NULL else eq$depends)
    src <- list(expression = jsonlite::unbox(eq$expr_str),
                line = jsonlite::unbox(eq$line))
  } else if (type == "dim") {
    user <- isTRUE(eq$rhs$user)
    if (user) {
      ## TODO: I think that this wants to be looked at again, but
      ## there's an issue here where we don't know where the data will
      ## be stored so it's hard to tell where it should be stored.
      value <- rep(list(NULL), eq$nd)
    } else if (eq$nd == 1) {
      value <- list(ir_expression(eq$rhs$value))
    } else {
      value <- lapply(eq$rhs$value[-1L], ir_expression)
    }
    rhs <- list(
      type = jsonlite::unbox(eq$rhs$type),
      value = value,
      user = jsonlite::unbox(user),
      depends = if (eq$rhs$type == "atomic") NULL else eq$depends)
    src <- list(expression = jsonlite::unbox(eq$expr_str),
                line = jsonlite::unbox(eq$line))
  } else if (type == "array_expression") {
    if (any(eq$rhs$inplace)) {
      stop("rhs$inplace")
    }
    rhs <- list(
      type = unname(eq$rhs$type),
      value = lapply(unname(eq$rhs$value), ir_expression),
      depends = if (all(eq$rhs$type == "atomic")) NULL else eq$depends)

    ## TODO: here we need to indicate if this has a *self-dependency*
    ## We can code generate some different codes here otherwise.
    lhs$index <- lapply(unname(eq$lhs$index), function(el)
      list(value = lapply(el$value, ir_expression),
           is_range = el$is_range,
           extent_min = lapply(el$extent_min, ir_expression),
           extent_max = lapply(el$extent_max, ir_expression)))

    ## TODO: this will change to just a set of linenumbers at some point
    src <- list(expression = eq$expr_str, line = eq$line)
  } else if (type == "user") {
    ## Here if there's a default it's a scalar expression for now!
    ##
    ## The atomic case is easy, and the *constant* case is probably
    ## not hard but I don't remember what else I supported here.  So
    ## until I remember let's just fail here.  Oddly this seems to be
    ## somewhat working for at least one user case where an atomic
    ## value is passed in - that suggests I may have an issue with the
    ## IR even?
    if (!is.null(eq$rhs$integer) || !is.null(eq$rhs$min) ||
         !is.null(eq$rhs$max)) {
      stop("User details need supporting")
    }
    rhs <- list(
      type = jsonlite::unbox(eq$rhs$type),
      value = if (eq$rhs$default) ir_expression(eq$rhs$value) else NULL,
      depends = if (eq$rhs$type == "atomic") NULL else eq$depends)
    src <- list(expression = jsonlite::unbox(eq$expr_str),
                line = jsonlite::unbox(eq$line))
  } else if (type == "interpolate") {
    rhs <- list(
      type = jsonlite::unbox(eq$rhs$type),
      value = ir_expression(eq$rhs$value),
      depends = eq$depends)
    src <- list(expression = jsonlite::unbox(eq$expr_str),
                line = jsonlite::unbox(eq$line))
  } else {
    stop("rhs type needs implementing")
  }

  list(name = jsonlite::unbox(eq$name),
       source = src,
       stage = jsonlite::unbox(STAGES[[eq$stage]]),
       type = jsonlite::unbox(type),
       stochastic = jsonlite::unbox(eq$stochastic),
       lhs = lhs,
       rhs = rhs)
}


ir_expression <- function(expr) {
  if (is.symbol(expr)) {
    jsonlite::unbox(as.character(expr))
  } else if (is.atomic(expr)) {
    jsonlite::unbox(expr)
  } else if (is.call(expr)) {
    c(list(jsonlite::unbox(as.character(expr[[1L]]))),
      lapply(expr[-1L], ir_expression))
  } else {
    stop("implement me")
  }
}

## This is the structure of data structures that desribe how data is stored
ir_data <- function(dat) {
  list(internal = ir_data_internal(dat),
       user = ir_data_user(dat),
       initial = ir_data_initial(dat),
       variable = ir_data_variable(dat, FALSE),
       output = ir_data_variable(dat, TRUE))
}


ir_data_internal <- function(dat) {
  if (!dat$info$discrete) {
    ## Add odin_use_dde as bool
  }

  ## TODO: dimensionscome through with the wrong data type and wrong
  ## rank here in parse - they should always be rank 0 integers.  Fix
  ## this in prep (it's not a big deal for R models I believe).

  ## TODO: interpolation allocation data type needs fixing: currently
  ## "interpolate" but we might use something more descriptive and
  ## representative of the C data type.

  ## if has delay add a ring
  i <- vcapply(dat$eqs, function(x) x$lhs$location) == "internal"
  data <- lapply(dat$eqs[i], function(eq)
    list(name = jsonlite::unbox(eq$lhs$name),
         stage = jsonlite::unbox(eq$stage),
         storage_type = jsonlite::unbox(eq$lhs$data_type),
         rank = jsonlite::unbox(eq$lhs$nd %||% 0L),
         transient = jsonlite::unbox(
           eq$stage == STAGE_TIME && !identical(eq$lhs$special, "initial"))))

  extra_dimensions <- function(eq) {
    f <- function(i) {
      list(name = jsonlite::unbox(array_dim_name(eq$lhs$name_target, i)),
           stage = jsonlite::unbox(eq$stage),
           storage_type = jsonlite::unbox("int"),
           rank = jsonlite::unbox(0L),
           transient = jsonlite::unbox(FALSE))
    }
    i <- seq_len(eq$nd)
    if (eq$nd >= 3) {
      i <- c(i, vcapply(3:eq$nd, function(i)
        paste(seq_len(i - 1), collapse = "")))
    }
    lapply(i, f)
  }

  dimensions <- unlist(lapply(unname(dat$eqs[dat$traits[, "is_dim"]]),
                              extra_dimensions), FALSE, FALSE)
  if (length(dimensions) > 0L) {
    data <- c(data, set_names(dimensions, vcapply(dimensions, "[[", "name")))
  }

  ## I am sure that there is more to add here - size, etc
  contents <- names_if(!vlapply(data, "[[", "transient"))

  ## We're dropping names here: the canonical name must now be the
  ## name element:
  stopifnot(
    identical(vcapply(data, "[[", "name", USE.NAMES = FALSE), names(data)))

  list(data = unname(data), contents = contents)
}


ir_data_user <- function(dat) {
  if (!dat$info$has_user) {
    return(NULL)
  }

  user <- dat$info$user
  stopifnot(identical(rownames(user), user$name))

  ## NOTE: min, max and default here are *optional* in the json
  ## because otherwise representing values is a little awkward.  Using
  ## infinities in particular don't serialise well at all!
  ##
  ## TODO: holding off on using min, max and integer because these
  ## belong more in the data declaration I think; we can use similar
  ## approaches to declare other constraints perhaps.
  f <- function(i) {
    ret <- list(name = jsonlite::unbox(user$name[[i]]),
                rank = jsonlite::unbox(user$rank[[i]]))
    ## NOTE: switching on this value is required for the case where we
    ## have array inputs because otherwise user$default_value[[i]] is
    ## a list of NULLs
    if (user$has_default[[i]]) {
      default <- user$default_value[[i]]
      if (ret$rank == 0L) {
        default <- jsonlite::unbox(default)
      }
      ret$default <- default
    }
    ret
  }

  lapply(seq_len(nrow(user)), f)
}


ir_data_initial <- function(dat) {
  list(stage = jsonlite::unbox(STAGES[[dat$info$initial_stage]]))
}


ir_data_variable <- function(dat, output) {
  if (output && !dat$info$has_output) {
    return(NULL)
  }

  info <- if (output) dat$output_info else dat$variable_info
  offset <- set_names(info$offset, info$order)
  rank <- set_names(info$array, info$order)

  ## For each here we store the data type only I think?  All ODE
  ## variables are constrained to be numeric, but that doesn't hold
  ## for discrete equations of course.

  ## TODO: This is one of the areas where the parse code needs
  ## completely refactoring to get us information in a better order.
  location <- if (output) "output" else "variable"
  i <- vcapply(dat$eqs, function(x) x$lhs$location) == location
  data <- lapply(dat$eqs[i], function(eq)
    list(name = jsonlite::unbox(eq$lhs$name_target),
         storage_type = jsonlite::unbox(eq$lhs$data_type),
         offset = jsonlite::unbox(offset[[eq$lhs$name_target]]),
         rank = jsonlite::unbox(rank[[eq$lhs$name_target]])))

  ## We require this to hold later:
  stopifnot(identical(vcapply(data, "[[", "name", USE.NAMES = FALSE),
                      info$order))
  names(data) <- NULL

  list(length = ir_expression(info$total),
       length_stage = jsonlite::unbox(info$total_stage),
       data = data)
}


ir_serialise <- function(dat, pretty = TRUE) {
  jsonlite::toJSON(dat, null = "null", pretty = pretty, digits = NA)
}


## TODO: we should be able to see in the schema all the cases that are
## character vectors.
ir_deserialise <- function(ir) {
  dat <- jsonlite::fromJSON(ir, simplifyVector = FALSE)
  dat$components <- lapply(dat$components, lapply, list_to_character)

  ## These are stored as an array (simpler in the schema, less
  ## duplication, order preserving) but we want to access by name:
  names(dat$data$internal$data) <-
    vcapply(dat$data$internal$data, "[[", "name")

  names(dat$data$variable$data) <-
    vcapply(dat$data$variable$data, "[[", "name")

  if (dat$features$has_output) {
    names(dat$data$output$data) <-
      vcapply(dat$data$output$data, "[[", "name")
  }

  dat
}
