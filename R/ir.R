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
                 equations = ir_equations(dat))
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

  ## TODO: use of "used" here is problematic because what we really
  ## mean here is "*changed*" - referencing things is just fine, but
  ## it's the point at which they are modified that we're trying to
  ## assess.

  ## rhs:
  ## * all equations with a time step?
  ## * all time-dependent dependencies of derivatives? [using this one]
  v <- names_if(dat$traits[, "is_deriv"])
  v_dep <- unique(unlist(dat$deps_rec[v], use.names = FALSE))
  eq_used_rhs <- set_names(
    names(dat$eqs) %in% c(v, v_dep[dat$stage[v_dep] == STAGE_TIME]),
    names(dat$eqs))
  var_used_rhs <- set_names(vars %in% v_dep, vars)

  ## output:
  v <- names_if(dat$traits[, "is_output"])
  v_dep <- unique(unlist(dat$deps_rec[v], use.names = FALSE))
  eq_used_output <- set_names(
    names(dat$eqs) %in% c(v, v_dep[dat$stage[v_dep] == STAGE_TIME]),
    names(dat$eqs))
  var_used_output <- set_names(vars %in% v_dep, vars)

  ## create:
  ## * all constant variables
  eq_used_create <- dat$stage[names(dat$eqs)] == STAGE_CONSTANT

  ## initial:
  v <- names_if(dat$traits[, "is_initial"])
  v_dep <- unique(c(v, unlist(dat$deps_rec[v], use.names = FALSE)))
  eq_used_initial <- set_names(
    names(dat$eqs) %in% v_dep[dat$stage[v_dep] == STAGE_TIME],
    names(dat$eqs))

  ## user:
  v <- setdiff(names_if(dat$stage[names(dat$eqs)] == STAGE_USER),
               names_if(dat$traits[, "uses_user"]))
  eq_used_user <- set_names(names(dat$eqs) %in% v, names(dat$eqs))

  used <- rbind(rhs = eq_used_rhs,
                output = eq_used_output,
                create = eq_used_create,
                user = eq_used_user,
                initial = eq_used_initial)

  for (i in seq_along(dat$eqs)) {
    dat$eqs[[i]]$lhs$location <- location[[dat$eqs[[i]]$name]]
    dat$eqs[[i]]$deps_rec <- dat$deps_rec[[i]] %||% stop("IR ERROR")
    dat$eqs[[i]]$used <- used[, i]
  }

  dat$variable_info$used <- rbind(rhs = var_used_rhs,
                                  output = var_used_output)

  dat
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
  if (dat$info$has_array && dat$info$has_user) {
    ## This is harder because it breaks the ordering code
    stop("check for and enforce user sized arrays")
  }
  lapply(dat$info[v], jsonlite::unbox)
}


ir_equations <- function(dat) {
  unname(lapply(dat$eqs, ir_equation))
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
  } else if (isTRUE(eq$rhs$interpolate)) {
    type <- "interoplate"
  } else if (isTRUE(eq$rhs$delay)) {
    type <- "delay"
  } else if (identical(eq$lhs$type, "symbol")) {
    type <- "scalar_expression"
  } else if (identical(eq$lhs$type, "array")) {
    type <- "array_expression"
  } else {
    stop("Unclassified type")
  }

  ## identical(eq$lhs$special, "dim")
  ## isTRUE(eq$rhs$interpolate)
  ## isTRUE(x$rhs$delay)

  if (type == "scalar_expression" || type == "dim") {
    rhs <- list(
      type = jsonlite::unbox(eq$rhs$type),
      value = ir_expression(eq$rhs$value),
      ## TODO: this conditional would be better in the parse?
      depends = if (eq$rhs$type == "atomic") NULL else eq$depends)
    src <- list(expression = jsonlite::unbox(eq$expr_str),
                line = jsonlite::unbox(eq$line))
  } else if (type == "array_expression") {
    if (any(eq$rhs$inplace)) {
      stop("rhs$inplace")
    }
    if (eq$lhs$nd > 1) {
      stop("multidimensional arrays need work")
    }
    rhs <- list(
      type = unname(eq$rhs$type),
      value = lapply(unname(eq$rhs$value), ir_expression),
      depends = if (all(eq$rhs$type == "atomic")) NULL else eq$depends)

    ## TODO: here we need to indicate if this has a *self-dependency*
    ## We can code generate some different codes here otherwise.
    lhs$index <- lapply(unname(eq$lhs$index), function(el)
      list(value = ir_expression(el$value[[1]]),
           is_range = jsonlite::unbox(el$is_range[[1]]),
           extent_min = ir_expression(el$extent_min[[1]]),
           extent_max = ir_expression(el$extent_max[[1]])))

    ## TODO: this will change to just a set of linenumbers at some point
    src <- list(expression = eq$expr_str, line = eq$line)
  } else {
    browser()
    stop("rhs type needs implementing")
  }

  list(name = jsonlite::unbox(eq$name),
       source = src,
       stage = jsonlite::unbox(STAGES[[eq$stage]]),
       type = jsonlite::unbox(type),
       used = lapply(eq$used, jsonlite::unbox),
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
    browser()
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

  ## if has delay add a ring
  i <- vcapply(dat$eqs, function(x) x$lhs$location) == "internal"
  data <- lapply(dat$eqs[i], function(eq)
    list(name = jsonlite::unbox(eq$lhs$name),
         storage_type = jsonlite::unbox(eq$lhs$data_type),
         stage = jsonlite::unbox(eq$stage),
         rank = jsonlite::unbox(eq$lhs$nd %||% 0L),
         transient = jsonlite::unbox(
           eq$stage == STAGE_TIME & !identical(eq$lhs$special, "initial"))))

  ## I am sure that there is more to add here - size, etc
  list(data = data)
}


ir_data_user <- function(dat) {
  if (!dat$info$has_user) {
    return(NULL)
  }

  user <- dat$info$user
  ## NOTE: min, max and default_value here are *optional* in the json
  ## because otherwise representing values is a little awkward.  Using
  ## infinities in particular don't serialise well at all!
  f <- function(i) {
    ret <- user[i, setdiff(names(user), "default_value")]
    if (!is.finite(ret$min)) {
      ret$min <- NULL
    }
    if (!is.finite(ret$max)) {
      ret$max <- NULL
    }
    ret <- lapply(ret, jsonlite::unbox)
    if (ret$has_default) {
      default_value <- user$default_value[[i]]
      if (ret$rank == 0L) {
        default_value <- jsonlite::unbox(default_value)
      }
      ret$default_value <- default_value
    }
    ret
  }
  set_names(lapply(seq_len(nrow(user)), f), rownames(user))
}


ir_data_initial <- function(dat) {
  list(stage = jsonlite::unbox(STAGES[[dat$info$initial_stage]]))
}


ir_data_variable <- function(dat, output) {
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
         stage = jsonlite::unbox(STAGE_TIME),
         transient = jsonlite::unbox(FALSE),
         offset = jsonlite::unbox(offset[[eq$lhs$name_target]]),
         rank = jsonlite::unbox(rank[[eq$lhs$name_target]]),
         ## NOTE: this silently falls through for outputs, which are
         ## not *used* like variables are, so for output = TRUE, used
         ## is *always* an empty list.
         used = lapply(info$used[, eq$lhs$name_target], jsonlite::unbox)))
  names(data) <- info$order

  list(order = info$order,
       length = ir_expression(info$total),
       length_stage = jsonlite::unbox(info$total_stage),
       length_is_var = jsonlite::unbox(info$total_is_var),
       data = data)
}


ir_serialise <- function(dat, pretty = TRUE) {
  jsonlite::toJSON(dat, null = "null", pretty = pretty, digits = NA)
}


ir_deserialise <- function(ir) {
  dat <- jsonlite::fromJSON(ir, simplifyVector = FALSE)
  dat$data$variable$order <- list_to_character(dat$data$variable$order)
  dat$data$output$order <- list_to_character(dat$data$output$order)
  dat
}
