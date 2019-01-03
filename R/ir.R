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
  v_dep <- unique(unlist(dat$deps_rec[v], use.names = FALSE))
  eq_used_initial <- set_names(
    names(dat$eqs) %in% c(v, v_dep[dat$stage[v_dep] == STAGE_TIME]),
    names(dat$eqs))

  used <- rbind(rhs = eq_used_rhs,
                output = eq_used_output,
                create = eq_used_create,
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
  if (!is.null(eq$lhs$special)) {
    lhs$special <- jsonlite::unbox(eq$lhs$special)
    lhs$target <- jsonlite::unbox(eq$lhs$name_target)
  }

  ## This is the major classification of types: things really route
  ## through different routes later based on this.  Later on we'll
  ## push some of the logic here into the parse functions I think
  if (identical(eq$lhs$special, "dim")) {
    type <- "dim"
  } else if (isTRUE(eq$rhs$interpolate)) {
    type <- "interoplate"
  } else if (isTRUE(eq$rhs$delay)) {
    type <- "delay"
  } else {
    ## TODO: I think that user() *must* be special really?
    type <- "expression"
  }

  if (eq$rhs$type == "atomic") {
    rhs <- list(type = jsonlite::unbox(eq$rhs$type),
                value = jsonlite::unbox(eq$rhs$value))
  } else if (eq$rhs$type == "expression") {
    rhs <- list(type = jsonlite::unbox(eq$rhs$type),
                value = ir_expression(eq$rhs$value),
                depends = eq$depends)
  } else {
    stop("rhs type needs implementing")
  }

  list(name = jsonlite::unbox(eq$name),
       source = list(expression = jsonlite::unbox(eq$expr_str),
                     line = jsonlite::unbox(eq$line)),
       stage = jsonlite::unbox(STAGES[[eq$stage]]),
       ## TODO: type should be in rhs I *think*
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
    stop("implement me")
  }
}

## This is the structure of data structures that desribe how data is stored
ir_data <- function(dat) {
  list(internal = ir_data_internal(dat),
       initial = ir_data_initial(dat),
       variable = ir_data_variable(dat),
       output = ir_data_output(dat))
}


ir_data_internal <- function(dat) {
  if (!dat$info$discrete) {
    ## Add odin_use_dde as bool
  }
  ## if has delay add a ring
  ## if arrays, add this here too!

  i <- vcapply(dat$eqs, function(x) x$lhs$location) == "internal"
  data <- lapply(dat$eqs[i], function(eq)
    list(name = jsonlite::unbox(eq$lhs$name),
         storage_type = jsonlite::unbox(eq$lhs$data_type),
         stage = jsonlite::unbox(eq$stage),
         rank = jsonlite::unbox(0L),
         transient = jsonlite::unbox(
           eq$stage == STAGE_TIME & !identical(eq$lhs$special, "initial"))))

  ## I am sure that there is more to add here - size, etc
  list(data = data)
}


ir_data_initial <- function(dat) {
  list(stage = jsonlite::unbox(STAGES[[dat$info$initial_stage]]))
}


ir_data_variable <- function(dat) {
  info <- dat$variable_info

  if (dat$info$has_array) {
    ## TODO: when there are arrays then the offset information becomes
    ## much more complicated.
    ## TODO: length needs adding if this is true
    stop("this needs work")
  }
  ## I am assuming this elsewhere!
  stopifnot(identical(dat$vars, info$order))

  nms <- target_name(info$order, dat$info$discrete)
  offset <- set_names(info$offset, nms)
  rank <- set_names(info$array, nms)

  ## For each here we store the data type only I think?  All ODE
  ## variables are constrained to be numeric, but that doesn't hold
  ## for discrete equations of course.


  ## TODO: This is one of the areas where the parse code needs
  ## completely refactoring to get us information in a better order.

  i <- vcapply(dat$eqs, function(x) x$lhs$location) == "variable"
  data <- lapply(dat$eqs[i], function(eq)
    list(name = jsonlite::unbox(eq$lhs$name_target),
         storage_type = jsonlite::unbox(eq$lhs$data_type),
         stage = jsonlite::unbox(STAGE_TIME),
         transient = jsonlite::unbox(FALSE),
         offset = jsonlite::unbox(offset[[eq$name]]),
         rank = jsonlite::unbox(rank[[eq$name]]),
         used = lapply(info$used[, eq$lhs$name_target], jsonlite::unbox)))
  names(data) <- dat$vars

  ## TODO: this doesn't support lots of things required to deal with
  ## variable length arrays, but length becomes an sexpr at some
  ## point.
  list(order = info$order,
       length = jsonlite::unbox(info$total),
       length_stage = jsonlite::unbox(info$total_stage),
       length_is_var = jsonlite::unbox(info$total_is_var),
       data = data)
}


ir_data_output <- function(dat) {
  if (!dat$info$has_output) {
    return(NULL)
  }
  stop("write ir_data_output")
}


ir_data1 <- function(eq) {
  if (identical(eq$lhs$special, "initial") || eq$stage < STAGE_TIME) {
    if (eq$lhs$type != "symbol") {
      stop("FIXME")
    }
    list(name = jsonlite::unbox(eq$name),
         storage_type = jsonlite::unbox(eq$lhs$data_type),
         rank = jsonlite::unbox(0L))
  }
}


## Eventually this should be cached within a session, but wait until
## it works.
ir_schema <- function() {
  path <- system.file("schema.json", package = "odin", mustWork = TRUE)
  dat <- readChar(path, file.size(path))
  ## We get somewhat better errors from jsonlite's parsers than hoping
  ## that the json is valid.
  jsonlite::fromJSON(dat)
  dat
}


ir_validate <- function(x, error = FALSE) {
  engine <- if (packageVersion("jsonvalidate") > "1.0.0") "ajv" else "imjv"
  jsonvalidate::json_validate(x, ir_schema(),
                              verbose = TRUE, greedy = TRUE, error = error,
                              engine = "imjv")
}


ir_serialise <- function(dat, pretty = TRUE) {
  jsonlite::toJSON(dat, null = "null", pretty = pretty)
}


ir_deserialise <- function(ir) {
  dat <- jsonlite::fromJSON(ir, simplifyVector = FALSE)
  dat$data$variable$order <- list_to_character(dat$data$variable$order)
  dat
}
