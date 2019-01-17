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
                 components = ir_components(dat),
                 source = vcapply(xp$exprs, deparse_str))
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
  ## Add extra equations
  dat <- ir_prep_offset(dat, FALSE)
  if (dat$info$has_output) {
    dat <- ir_prep_offset(dat, TRUE)
  }

  if (dat$info$has_array) {
    dat <- ir_prep_dim(dat)
  }

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
    tmp <- dat$traits[i, , drop = FALSE]
    rownames(tmp) <- names(alloc)
    dat$traits <- rbind(dat$traits, tmp)
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


ir_prep_offset <- function(dat, output) {
  info <- if (output) dat$output_info else dat$variable_info
  i <- vlapply(info$offset, is.language)
  if (any(i)) {
    ## Here, add auxillary equations into the right place - eventually
    ## we'll do this directly from odin's parse!  Things that need
    ## modifying:
    ##
    ## - traits
    ## - eqs
    ## - stage
    ## - deps_rec
    ##
    ## base the offsets off of the dimension of the array being worked
    ## with in terms of position in the graph, but make sure that the
    ## stage reflects their dependenies and not the target.
    ##
    ## We'll base these off of the equation that defines the variable
    ## I think, because that's the equation that requires the
    ## existance of the offset.
    eqs <- lapply(which(i), ir_prep_offset1, info, dat)
    stage <- viapply(eqs, "[[", "stage")
    deps_rec <- lapply(eqs, function(x)
      sort(unique(unlist(dat$deps_rec[x$depends$variables]), FALSE, FALSE)))

    traits <- dat$traits[seq_along(eqs), , drop = FALSE]
    traits[] <- FALSE
    traits[, "is_symbol"] <- TRUE
    rownames(traits) <- names(eqs)

    dat$eqs <- c(dat$eqs, eqs)
    dat$stage <- c(dat$stage, stage)
    dat$deps_rec <- c(dat$deps_rec, deps_rec)
    dat$traits <- rbind(dat$traits, traits)
  }
  dat
}


ir_prep_offset1 <- function(i, info, dat) {
  nm <- names(info$offset)[[i]]
  value <- info$offset[[i]]
  depends <- find_symbols(value)
  parent <- dat$eqs[[info$names[[i]]]]
  stopifnot(!is.null(parent))
  stage <- max(dat$stage[depends$variables])
  list(name = nm,
       lhs = list(type = "symbol",
                  name = nm,
                  data_type = "int"),
       rhs = list(type = "expression",
                  value = value,
                  depends = depends),
       depends = depends,
       stochastic = FALSE,
       expr = parent$expr,
       expr_str = parent$expr_str,
       line = parent$line,
       stage = stage)
}


ir_prep_interleave_index <- function(i, m) {
  stopifnot(sum(i) == length(m))
  j <- c(seq_along(i), rep(unname(which(i)), m))
  j[which(i)] <- NA
  order(j, na.last = FALSE)[-seq_len(sum(i))]
}


ir_prep_dim_user <- function(nm, dat) {
  ## if we run this lot in set user then I think that it will work -
  ## it needs to really for this to all make any sense!  So let's give
  ## it look.  We will rewrite:

  ## * move the target equation _before_ the user provided one
  ## * rewrite the dependencies to remove the dependency on the dimension
  eq <- dat$eqs[[nm]]
  eq <- list(
    name = nm,
    lhs = list(type = "null", name = nm, nd = eq$lhs$nd,
               data_type = eq$lhs$data_type),
    rhs = list(type = "null", value = NULL, depends = NULL),
    depends = NULL,
    stochastic = FALSE,
    expr = eq$expr,
    expr_str = eq$expr_str,
    line = eq$line,
    stage = eq$stage)
  dat$eqs[[nm]] <- eq

  ## Then reorder things a little:
  i <- which(names(dat$eqs) == nm)
  j <- which(names(dat$eqs) == array_dim_name(nm))
  stopifnot(i > j)

  k <- seq_along(dat$eqs)[-i]
  k <- c(k[seq_len(j - 1)], i, k[-seq_len(j - 1)])
  dat$eqs <- dat$eqs[k]
  dat
}


ir_prep_dim <- function(dat) {
  i <- dat$traits[, "is_dim"]
  if (any(i)) {
    dim_user <- unlist(lapply(dat$eqs[i], function(x)
      if (isTRUE(x$rhs$user)) x$lhs$name_target), FALSE, FALSE)
    ## Here we just need to expand out the equations and the
    ## associated other bits of the data (stage, deps_rec and traits).
    ## This is a fiddle but not that hard.  Once that's done, we might
    ## chase this through the generation then look at the user sized
    ## arrays which need the f1 part of this done differently (and
    ## that changes a few things but not a massive amount).
    tmp <- lapply(dat$eqs[i], ir_prep_dim1, dat)
    eqs <- unlist(tmp, FALSE, FALSE)
    names(eqs) <- vcapply(eqs, "[[", "name")
    eqs_len <- lengths(tmp, FALSE)

    ## So here we add everything - this is going to differ for the
    ## user dimension case again, but we can work around that as
    ## needed.
    traits <- dat$traits[rep(1, length(eqs)), , drop = FALSE]
    traits[] <- FALSE
    traits[, "is_symbol"] <- TRUE
    rownames(traits) <- names(eqs)
    stage <- viapply(eqs, "[[", "stage")
    deps_rec <- lapply(eqs, function(x)
      sort(unique(unlist(dat$deps_rec[x$depends$variables]), FALSE, FALSE))
      %||% character(0))

    ## Now interleave:
    j <- ir_prep_interleave_index(i, eqs_len)
    dat$eqs <- c(dat$eqs, eqs)[j]
    dat$traits <- rbind(dat$traits, traits)[j, , drop = FALSE]

    dat$stage <- c(dat$stage[!i], stage)
    dat$deps_rec <- c(dat$deps_rec[!i], deps_rec)

    ## Here we need to move these to just before their respective
    ## assigments:
    for (nm in dim_user) {
      dat <- ir_prep_dim_user(nm, dat)
    }
  }
  dat
}


## TODO: this needs to be different for rank 1 because the whole thing
## is much simpler.  but it still needs working on...
ir_prep_dim1 <- function(eq, dat) {
  rank <- eq$nd
  name <- eq$lhs$name_target
  dims <- lapply(seq_len(rank), function(i) as.name(array_dim_name(name, i)))

  f <- function(dim_name, value) {
    if (is.null(value)) {
      type <- "expression"
      depends <- find_symbols(as.name(name))
      value <- as.call(c(list(quote(user), as.name(name)), dims))
    } else {
      type <- if (is.atomic(value)) "atomic" else "expression"
      depends <- find_symbols(value)
    }
    list(name = dim_name,
         lhs = list(type = "symbol",
                    name = dim_name,
                    data_type = "int"),
         rhs = list(type = type,
                    value = value,
                    depends = depends),
         depends = depends,
         stochastic = FALSE,
         expr = eq$expr,
         expr_str = eq$expr_str,
         line = eq$line,
         stage = eq$stage)
  }

  ## Primary dimensions (will be done differently in the case of user
  ## dimensions)
  f1 <- function(i) {
    f(array_dim_name(name, i), eq$rhs$value[[i + 1]])
  }

  f2 <- function(i) {
    j <- seq_len(i - 1)
    name <- array_dim_name(name, paste(j, collapse = ""))
    value <- collapse_expr(dims[j], "*")
    f(name, value)
  }

  f3 <- function(i) {
    f(array_dim_name(name, i), 0L)
  }

  if (eq$rhs$user) {
    ## NOTE: it would be really nice to stop the assignments to basic
    ## dimensions these even being run, but that's not straightforward
    ## at this point because equation use is how we currently work out
    ## what the variables are.  We could strip these right out of the
    ## IR but I think that it's better to start with initialising
    ## things in general (especially with the C version - otherwise we
    ## should get a compiler warning that we're passing an
    ## uninitialised pointer around).  We could also strip out all
    ## null equations here but I think for now we can just ignore this
    ## weirdness and deal with it when the parse->ir code gets
    ## refactored.
    ret <- c(lapply(seq_len(eq$nd), f3),
             list(f(array_dim_name(name), NULL)))
  } else {
    ret <- c(lapply(seq_len(eq$nd), f1),
             list(f(array_dim_name(name), collapse_expr(dims, "*"))))
  }

  if (rank >= 3) {
    ret <- c(ret, lapply(3:eq$nd, f2))
  }

  ret
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
  exclude <- vlapply(dat$eqs, function(x) isTRUE(x$exclude))
  unname(lapply(dat$eqs[!exclude], ir_equation))
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
  if (is.null(eq$lhs$special) || eq$lhs$special == "initial") {
    lhs$target <- jsonlite::unbox(eq$name)
  } else {
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
    stop("delays not yet supported")
    type <- "delay"
  } else if (identical(eq$lhs$type, "symbol")) {
    type <- "scalar_expression"
  } else if (identical(eq$lhs$type, "array")) {
    type <- "array_expression"
  } else if (identical(eq$lhs$type, "null")) {
    type <- "null"
  } else {
    stop("Unclassified type")
  }

  ## TODO: this should be simplified away later in the parse because I
  ## know that we have all this information.  In this case the "type"
  ## earlier should be set to copy and we should no expand out all the
  ## rhs lines.  However, there are more expression types that can be
  ## thought of as copies I think so this might be generally useful
  ## (anything of the form x[] <- b[i] is just a copy).
  ##
  ## TODO: I don't see that the second value of this is working
  ## (checking that the value is the same as the target) because the
  ## value is a symbol/language object.
  if (identical(eq$lhs$special, "output")) {
    if (type == "array_expression" &&
        length(eq$rhs$value) == 1L &&
        (isTRUE(eq$rhs$value[[1]]) ||
         eq$rhs$value[[1]] == eq$lhs$name_target)) {
      type <- "copy"
    }
    if (type == "scalar_expression" && isTRUE(eq$rhs$value)) {
      type <- "copy"
    }
  }

  if (type == "null") {
    rhs <- list(
      type = jsonlite::unbox("null"),
      value = NULL)
    depends <- NULL
  } else if (type == "scalar_expression") {
    rhs <- list(
      type = jsonlite::unbox(eq$rhs$type),
      value = ir_expression(eq$rhs$value))
    depends <- if (eq$rhs$type == "atomic") NULL else eq$depends
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
      user = jsonlite::unbox(user))
    depends <- if (eq$rhs$type == "atomic") NULL else eq$depends
  } else if (type == "array_expression") {
    if (any(eq$rhs$inplace)) {
      stop("rhs$inplace")
    }
    rhs <- list(
      type = unname(eq$rhs$type),
      value = lapply(unname(eq$rhs$value), ir_expression))
    depends <- if (all(eq$rhs$type == "atomic")) NULL else eq$depends

    ## TODO: here we need to indicate if this has a *self-dependency*
    ## We can code generate some different codes here otherwise.
    lhs$index <- lapply(unname(eq$lhs$index), function(el)
      list(value = lapply(el$value, ir_expression),
           is_range = el$is_range,
           extent_min = lapply(el$extent_min, ir_expression),
           extent_max = lapply(el$extent_max, ir_expression)))
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
      value = if (eq$rhs$default) ir_expression(eq$rhs$value) else NULL)
    depends <- if (eq$rhs$type == "atomic") NULL else eq$depends
  } else if (type == "interpolate") {
    rhs <- list(
      type = jsonlite::unbox(eq$rhs$type),
      value = ir_expression(eq$rhs$value))
    depends <- eq$depends
  } else if (type == "copy") {
    rhs <- list(type = jsonlite::unbox("copy"),
                value = jsonlite::unbox(eq$lhs$name_target))
    depends <- NULL
  } else {
    stop("rhs type needs implementing")
  }

  list(name = jsonlite::unbox(eq$name),
       source = eq$line,
       depends = depends, # TODO
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
         storage_type = jsonlite::unbox(eq$lhs$data_type),
         rank = jsonlite::unbox(eq$lhs$nd %||% 0L),
         user = jsonlite::unbox(eq$lhs$type == "array" &&
                                eq$rhs$type == "null"),
         transient = jsonlite::unbox(eq$stage == STAGE_TIME &&
                                     !identical(eq$lhs$special, "initial"))))

  extra_dimensions <- function(eq) {
    f <- function(i) {
      list(name = jsonlite::unbox(array_dim_name(eq$lhs$name_target, i)),
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
    list(name = jsonlite::unbox(user$name[[i]]),
         rank = jsonlite::unbox(user$rank[[i]]),
         has_default = jsonlite::unbox(user$has_default[[i]]))
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
         offset = ir_expression(offset[[eq$lhs$name_target]]),
         rank = jsonlite::unbox(rank[[eq$lhs$name_target]])))

  ## We require this to hold later:
  nms <- vcapply(data, "[[", "name", USE.NAMES = FALSE)
  stopifnot(identical(sort(nms), sort(info$order)))
  data <- unname(data[match(info$order, nms)])

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
