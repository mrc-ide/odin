generate_r <- function(dat, opts) {
  if (dat$features$has_delay) {
    ## We're going to need an additional bit of internal data here,
    ## but this sits outside the core odin ir
    dat$meta$use_dde <- "odin_use_dde"
    dat$data$elements[[dat$meta$use_dde]] <- list(name = dat$meta$use_dde,
                                                  location = "internal",
                                                  storage_type = "bool",
                                                  rank = 0L,
                                                  dimnames = NULL)
  }

  dat$meta$support <-
    list(get_user_double = "_get_user_double",
         get_user_dim = "_get_user_dim",
         check_interpolate_y = "_check_interpolate_y",
         check_interpolate_t = "_check_interpolate_t")

  ## This is our little rewriter - we'll tidy this up later
  rewrite <- function(x) {
    generate_r_sexp(x, dat$data, dat$meta)
  }

  eqs <- generate_r_equations(dat, rewrite)

  ## Then start putting together the initial conditions
  env <- new.env(parent = odin_base_env())

  ## Support functions will come in this way:
  if (dat$features$has_user) {
    env[[dat$meta$support$get_user_double]] <- support_get_user_double
    env[[dat$meta$support$get_user_dim]] <- support_get_user_dim
  }
  if (dat$features$has_interpolate) {
    env[[dat$meta$support$check_interpolate_y]] <- support_check_interpolate_y
    env[[dat$meta$support$check_interpolate_t]] <- support_check_interpolate_t
  }

  core <- generate_r_core(eqs, dat, env, rewrite)
  generate_r_class(core, dat, env)
}


generate_r_core <- function(eqs, dat, env, rewrite) {
  core <- list(
    create = generate_r_create(eqs, dat, env),
    ic = generate_r_ic(eqs, dat, env, rewrite),
    set_user = generate_r_set_user(eqs, dat, env),
    rhs_desolve = generate_r_rhs(eqs, dat, env, rewrite, "desolve"),
    rhs_dde = generate_r_rhs(eqs, dat, env, rewrite, "dde"),
    output = generate_r_rhs(eqs, dat, env, rewrite,"output"),
    interpolate_t = generate_r_interpolate_t(dat, env, rewrite),
    set_initial = generate_r_set_initial(dat, env, rewrite),
    run = generate_r_run(dat, env, rewrite),
    metadata = generate_r_metadata(dat, rewrite))
  list2env(core, env)
  core
}


generate_r_create <- function(eqs, dat, env) {
  alloc <- call("<-", as.name(dat$meta$internal),
                quote(new.env(parent = emptyenv())))
  eqs_create <- flatten_eqs(eqs[dat$components$create$equations])
  ret <- as.name(dat$meta$internal)
  body <- as.call(c(list(quote(`{`)), c(alloc, eqs_create, ret)))
  as_function(alist(), body, env)
}


## TODO: 'ic' ==> 'initial'
generate_r_ic <- function(eqs, dat, env, rewrite) {
  ## Equations to run before initial conditions are computed:
  eqs_initial <- flatten_eqs(eqs[dat$components$initial$equations])

  ## We need a little fiction here because any use of a variable must
  ## use its "initial" name at this point.  We could filter through
  ## dependencies and work out if this is necessary, but this should
  ## be fairly harmless, and at the moment we don't report this well.
  subs <- lapply(dat$data$variable$contents, function(x) rewrite(x$initial))
  eqs_initial <- lapply(eqs_initial, substitute_, as.environment(subs))

  ## Allocate space for the state vector
  state <- as.name(dat$meta$state)
  var_length <- rewrite(dat$data$variable$length)
  alloc <- call("<-", state, call("numeric", var_length))

  ## Assign into the state vector
  f <- function(x) {
    d <- dat$data$elements[[x$name]]
    if (d$rank == 0L) {
      target <- call("[[", state, offset_to_position(x$offset))
    } else {
      offset <- rewrite(x$offset)
      seq <- call("seq_len", rewrite(d$dimnames$length))
      target <- call("[", state, call("+", offset, seq))
    }
    call("<-", target, rewrite(x$initial))
  }
  assign <- lapply(dat$data$variable$contents, f)

  ## Build the function:
  body <- as.call(c(list(quote(`{`)), eqs_initial, alloc, assign, state))
  args <- alist(time =, internal =)
  names(args)[[1]] <- dat$meta$time
  names(args)[[2]] <- dat$meta$internal
  as_function(args, body, env)
}


generate_r_set_user <- function(eqs, dat, env) {
  eqs_user <- flatten_eqs(eqs[dat$components$user$equations])
  args <- alist(user =, internal =)
  names(args)[[1]] <- dat$meta$user
  names(args)[[2]] <- dat$meta$internal

  user <- vcapply(dat$user, "[[", "name")
  unknown <- call("<-", quote(unknown),
                  call("setdiff",
                       call("names", as.name(dat$meta$user)),
                       as.call(c(quote(c), user))))
  check <- call(
    "if",
    quote(length(unknown) > 0L),
    expr_block(quote(stop("Unknown user parameters: ",
                          paste(unknown, collapse = ", ")))))

  body <- expr_block(c(list(unknown, check, eqs_user)))
  as_function(args, body, env)
}


generate_r_rhs <- function(eqs, dat, env, rewrite, rhs_type) {
  discrete <- dat$features$discrete
  has_output <- dat$features$has_output

  if (rhs_type == "output" && !has_output) {
    return(NULL)
  }
  if (discrete && rhs_type != "dde") {
    return(NULL)
  }

  ## This bit is surprisingly hard:
  if (discrete || rhs_type == "desolve") {
    use <- c("rhs", "output")
  } else {
    use <- if (rhs_type == "dde") "rhs" else "output"
  }
  join <- function(x, nm) {
    if (length(x) == 1L) x[[1]][[nm]] else union(x[[1]][[nm]], x[[2]][[nm]])
  }
  use_vars <- join(dat$components[use], "variables")
  use_eqs <- join(dat$components[use], "equations")

  ## NOTE: this is really similar to code in ic but that goes the
  ## other way, into the state vector.
  f <- function(x) {
    d <- dat$data$elements[[x$name]]
    state <- as.name(dat$meta$state)
    if (d$rank == 0L) {
      extract <- call("[[", state, offset_to_position(x$offset))
    } else {
      seq <- call("seq_len", rewrite(d$dimnames$length))
      extract <- call("[", state, call("+", rewrite(x$offset), seq))
      if (d$rank > 1L) {
        extract <- call("array", extract, generate_r_dim(d, rewrite))
      }
    }
    call("<-", as.name(x$name), extract)
  }

  vars <- unname(drop_null(lapply(dat$data$variable$contents[use_vars], f)))

  ## NOTE: There are two reasonable things to do here - we can look up
  ## the length of the variable (dat$data$variable$length) or we can
  ## just make this a vector the same length as the incoming state (as
  ## dydt is always the same length as y).  Neither seems much better
  ## than the other, so going with the same length approach here as it
  ## is less logic and will work for variable-length cases.
  alloc_result <- call("<-", as.name(dat$meta$result),
                       call("numeric", call("length", as.name(dat$meta$state))))

  ## For output_length we have no real choice but to look up the
  ## length each time.
  alloc_output <- call("<-", as.name(dat$meta$output),
                       call("numeric", rewrite(dat$data$output$length)))
  alloc <- list(rhs = alloc_result, output = alloc_output)[use]

  result <- as.name(dat$meta$result)
  output <- as.name(dat$meta$output)

  if (rhs_type == "desolve") {
    if (has_output) {
      ret <- call("list", result, output)
    } else {
      ret <- call("list", result)
    }
  } else if (rhs_type == "output") {
    ret <- output
  } else {
    if (discrete && has_output) {
      ret <- list(
        call("<-", call("attr", result, "output"), output),
        result)
    } else {
      ret <- result
    }
  }

  eqs_include <- flatten_eqs(eqs[use_eqs])
  body <- as.call(c(list(quote(`{`)), c(vars, alloc, eqs_include, ret)))
  args <- alist(t = , y =, parms = )
  names(args)[[1]] <- dat$meta$time
  names(args)[[2]] <- dat$meta$state
  names(args)[[3]] <- dat$meta$internal
  as_function(args, body, env)
}


generate_r_metadata <- function(dat, rewrite) {
  ord <- function(location) {
    contents <- dat$data$elements[names(dat$data[[location]]$contents)]
    len <- lapply(contents, generate_r_dim, rewrite)
    as.call(c(list(quote(list)), len))
  }

  ynames <- call(
    "make_names",
    quote(private$variable_order), quote(private$output_order),
    dat$features$discrete)
  n_out <- quote(support_n_out(private$output_order))

  env <- new.env(parent = environment(odin))

  body <- list(
    call("<-", as.name(dat$meta$internal), quote(private$data)),
    call("<-", quote(private$variable_order), ord("variable")),
    call("<-", quote(private$output_order), ord("output")),
    call("<-", call("$", quote(private), quote(ynames)), ynames),
    call("<-", call("$", quote(private), quote(n_out)), n_out))

  if (dat$features$has_interpolate) {
    body <- c(
      body,
      quote(private$interpolate_t <- private$core$interpolate_t(private$data)))
  }

  args <- alist(self =, private =)
  body <- as.call(c(list(as.name("{")), body))

  as_function(args, body, env)
}


generate_r_interpolate_t <- function(dat, env, rewrite) {
  if (!dat$features$has_interpolate) {
    return(function(...) NULL)
  }

  args_min <- lapply(dat$interpolate$min, function(x)
    call("[[", rewrite(x), 1L))
  if (length(args_min) == 1L) {
    min <- args_min[[1L]]
  } else {
    min <- as.call(c(list(quote(max)), args_min))
  }

  args_max <- lapply(dat$interpolate$max, function(x)
    call("[[", rewrite(x), call("length", rewrite(x))))
  if (length(args_max) == 0L) {
    max <- Inf
  } else if (length(args_max) == 1L) {
    max <- args_max[[1L]]
  } else {
    max <- as.call(c(list(quote(min)), args_max))
  }

  args_critical <- lapply(dat$interpolate$critical, rewrite)
  if (length(args_critical) == 0L) {
    critical <- numeric(0)
  } else if (length(args_critical) == 1L) {
    critical <- args_critical[[1L]]
  } else {
    critical <-
      call("sort", call("unique", as.call(c(list(quote(c)), args_critical))))
  }

  args <- set_names(alist(internal = ), dat$meta$internal)
  body <- call("{",
               call("list",
                    min = min, max = max, critical = critical))
  as_function(args, body, env)
}


generate_r_set_initial <- function(dat, env, rewrite) {
  if (!dat$features$has_delay) {
    return(function(...) NULL)
  }

  set_y <- call(
    "if", call("!", call("is.null", as.name(dat$meta$state))),
    expr_block(lapply(dat$data$variable$contents, function(x)
      call("<-", rewrite(x$initial),
           extract_variable(x, dat$data$elements, as.name(dat$meta$state),
                            rewrite)))))
  set_t <- call("<-", rewrite(dat$meta$initial_time), as.name(dat$meta$time))

  body <- list(set_y, set_t)
  args <- set_names(
    alist(, , ),
    c(dat$meta$time, dat$meta$state, dat$meta$internal))

  if (!dat$features$discrete) {
    args <- c(args, set_names(alist(x = ), dat$meta$use_dde))
    body <- c(body, list(call("<-", rewrite(dat$meta$use_dde),
                              as.name(dat$meta$use_dde))))
  }

  as_function(args, expr_block(body), env)
}


## Thos feels pretty messy, but I think we can clean it up later.
## It's quite likely that we'd be better off with two separate
## generators - one for discrete and one for continuous models.
generate_r_run <- function(dat, env, rewrite) {
  args <- alist(data =, t =, y = , n_out =, ynames =, ...=,
                  ## These are only used in some cases!
                  interpolate_t =)
  if (dat$features$discrete) {
    args <- c(args, alist(replicate =))
  } else {
    args <- c(args, alist(use_dde =, tcrit =))
  }
  if (!dat$features$discrete && dat$features$has_delay) {
    args <- c(args, alist(n_history = 1000L))
  }

  names(args)[[1L]] <- dat$meta$internal
  names(args)[[2L]] <- dat$meta$time
  names(args)[[3L]] <- dat$meta$state
  ## for now at least, quote these - we might have to fix this up
  ## later by having a second (non-ir driven) meta list.
  n_out <- quote(n_out)
  ynames <- quote(ynames)
  tcrit <- if (dat$features$discrete) NULL else quote(tcrit)

  replicate <- quote(replicate)
  use_dde <- quote(use_dde)
  n_history <- quote(n_history)
  interpolate_t <- quote(interpolate_t)

  ret <- quote(ret)

  t0 <- call("[[", as.name(dat$meta$time), 1L)

  if (dat$features$has_delay) {
    set_initial <- list(quote(set_initial), t0, as.name(dat$meta$state),
                        as.name(dat$meta$internal))
    if (!dat$features$discrete) {
      set_initial <- c(set_initial, list(use_dde))
    }
    set_initial <- as.call(set_initial)
  } else {
    set_initial <- NULL
  }

  if (dat$features$has_interpolate) {
    check_interpolate_t <-
      call(dat$meta$support$check_interpolate_t,
           as.name(dat$meta$time), interpolate_t, tcrit)
    if (!dat$features$discrete) {
      check_interpolate_t <- call("<-", tcrit, check_interpolate_t)
    }
  } else {
    check_interpolate_t <- NULL
  }

  compute_initial <-
    call("if", call("is.null", as.name(dat$meta$state)),
         expr_block(call("<-", as.name(dat$meta$state),
                         call("ic", t0, as.name(dat$meta$internal)))))

  output <- if (dat$features$has_output) quote(output) else NULL

  run_args_dde <- list(as.name(dat$meta$state), as.name(dat$meta$time),
                       quote(rhs_dde), as.name(dat$meta$internal),
                       ynames = FALSE, quote(`...`))
  run_args_desolve <- list(as.name(dat$meta$state), as.name(dat$meta$time),
                           quote(rhs_desolve), as.name(dat$meta$internal),
                           quote(`...`))
  if (!dat$features$discrete) {
    run_args_desolve <- c(run_args_desolve, tcrit = tcrit)
  }
  if (dat$features$has_output) {
    run_args_dde <- c(run_args_dde, list(n_out = n_out))
    if (!dat$features$discrete) {
      run_args_dde <- c(run_args_dde, list(output = quote(output)))
    }
  }
  if (!dat$features$discrete && dat$features$has_delay) {
    run_args_dde <- c(run_args_dde, list(n_history = n_history))
    run_args_desolve <- c(run_args_desolve,
                          list(control = call("list", mxhist = n_history)))
  }

  if (dat$features$discrete) {
    run_replicate <-
      as.call(c(list(quote(dde::difeq_replicate), replicate), run_args_dde))
    run_single <-
      as.call(c(list(quote(dde::difeq)), run_args_dde))
    run <- expr_if(
      call("is.null", replicate),
      call("<-", ret, run_single),
      call("<-", ret, run_replicate))
  } else {
    run_fn_dde <- quote(dde::dopri)
    if (dat$features$has_delay) {
      run_fn_desolve <- quote(deSolve::dede)
    } else {
      run_fn_desolve <- quote(deSolve::ode)
    }
    run <- expr_if(
      use_dde,
      call("<-", ret, as.call(c(list(run_fn_dde), run_args_dde))),
      call("<-", ret, as.call(c(list(run_fn_desolve), run_args_desolve))))
  }

  set_names <- expr_if(
    call("is.null", ynames),
    list(call("<-", call("colnames", ret), NULL),
         call("<-", call("class", ret), "matrix")),
    call("<-", call("colnames", ret), ynames))


  body <- drop_null(list(set_initial, check_interpolate_t, compute_initial, run,
                         set_names, ret))

  as_function(args, expr_block(body), env)
}


generate_r_dim <- function(data_info, rewrite) {
  if (data_info$rank == 0L) {
    NULL
  } else if (data_info$rank == 1L) {
    rewrite(data_info$dimnames$length)
  } else {
    as.call(c(list(quote(c)), lapply(data_info$dimnames$dim, rewrite)))
  }
}


## Some support functions - these are not subject to code generation
## at all and will be injected into the appropriate environment.
support_get_user_double <- function(user, name, internal, size, default,
                                    min, max, integer) {
  value <- user[[name]]
  if (is.null(value)) {
    if (is.null(internal[[name]])) {
      if (is.null(default)) {
        stop(sprintf("Expected a value for '%s'", name), call. = FALSE)
      } else {
        value <- default
      }
    } else {
      ## This has the slightly annoying property of setting the value
      ## to itself but that's harmless in the face of other
      ## inefficiencies and preserves this as a pure function.
      value <- internal[[name]]
    }
  } else {
    value <- support_coerce_mode(value, integer, min, max, name)
    d <- dim(value)
    if (is.null(size)) {
      if (length(value) != 1L || !is.null(d)) {
        stop(sprintf("Expected a scalar numeric for '%s'", name), call. = FALSE)
      }
    } else if (length(size) == 1L) {
      if (length(value) != size || !is.null(d)) {
        stop(sprintf("Expected length %d value for %s", size, name),
             call. = FALSE)
      }
    } else {
      if (length(d) != length(size) || any(d != size)) {
        stop(sprintf("Expected a numeric array with dimensions %s for '%s'",
                     paste(size, collapse = " * "), name), call. = FALSE)
      }
    }
  }
  value
}


support_check_interpolate_t <- function(time, dat, tcrit) {
  if (time[[1]] < dat$min) {
    stop(sprintf("Integration times do not span interpolation range; min: %s",
                 dat$min), call. = FALSE)
  }
  if (time[[length(time)]] > dat$max) {
    stop(sprintf("Integration times do not span interpolation range; max: %s",
                 dat$max), call. = FALSE)
  }
  if (length(dat$max) > 0L && is.null(tcrit)) {
    ## min(dat$max, tcrit) # TODO: breaks tests, but better behaviour
    dat$max
  } else {
    tcrit
  }
}


## This one works entirely through side effects to avoid the confusion
## and any ambiguity about what is set where.
support_get_user_dim <- function(user, internal, name, len, dims,
                                 min, max, integer) {
  value <- user[[name]]
  if (is.null(value) && !is.null(internal[[name]])) {
    ## Leave previous value alone:
    return()
  }
  if (is.null(value)) {
    stop(sprintf("Expected a value for '%s'", name), call. = FALSE)
  }
  value <- support_coerce_mode(value, integer, min, max, name)
  d <- dim(value)
  if (is.null(dims)) {
    if (!is.null(d)) {
      stop(sprintf("Expected a numeric vector for '%s'", name),
           call. = FALSE)
    }
  } else {
    rank <- length(dims)
    if (length(d) != rank) {
      stop(sprintf("Expected a numeric array of rank %d for '%s'", rank, name),
           call. = FALSE)
    }
    for (i in seq_len(rank)) {
      internal[[dims[[i]]]] <- d[[i]]
    }
  }
  internal[[len]] <- length(value)
  internal[[name]] <- value
}


support_check_interpolate_y <- function(dim_arg, dim_target, name_arg,
                                        name_target) {
  rank <- length(dim_target) - 1L
  stopifnot(length(dim_target) == length(dim_arg))
  if (rank == 0L) {
    if (dim_arg != dim_target) {
      stop(sprintf("Expected %s to have length %d (for %s)",
                   name_arg, dim_target, name_target), call. = FALSE)
    }
  } else {
    ## TODO: this can be simplifed after tests are passing; we want to
    ## match errors at the moment.
    i <- dim_arg != dim_target
    if (any(i)) {
      j <- which(i)[[1L]]
      stop(sprintf("Expected dimension %d of %s to have size %d (for %s)",
                   j, name_arg, dim_target[[j]], name_target),
           call. = FALSE)
    }
  }
}


offset_to_position <- function(x) {
  if (is.language(x)) {
    call("+", x, 1L)
  } else {
    x + 1L
  }
}


collapse_expr <- function(expr, join) {
  ret <- expr[[1L]]
  for (i in seq_along(expr)[-1L]) {
    ret <- call(join, ret, expr[[i]])
  }
  ret
}


## In an effort to build the minimal set of functions needed, here is
## a fully "known" set of functions.  This will grow as more functions
## are added but stops the issue of not explicitly supporting
## functions and getting drift between different implementations.
## Eventually even 'base' might want to change here.
odin_base_env <- function() {
  env <- new.env(parent = as.environment("package:base"))

  stats <- as.environment("package:stats")
  imports <- grep("_rand$", names(FUNCTIONS_STOCHASTIC),
                  invert = TRUE, value = TRUE)
  for (i in imports) {
    env[[i]] <- stats[[i]]
  }

  env
}


flatten_eqs <- function(x) {
  x <- unname(x)
  if (any(vlapply(x, is.list))) {
    x <- unlist(x, FALSE, FALSE)
  }
  x
}


unpack_variable <- function(x, data, state, rewrite) {
  call("<-", as.name(x$name), extract_variable(x, data, state, rewrite))
}


extract_variable <- function(x, data, state, rewrite) {
  d <- data[[x$name]]
  if (d$rank == 0L) {
    extract <- call("[[", state, offset_to_position(x$offset))
  } else {
    seq <- call("seq_len", rewrite(d$dimnames$length))
    extract <- call("[", state, call("+", rewrite(x$offset), seq))
    if (d$rank > 1L) {
      extract <- call("array", extract, generate_r_dim(d, rewrite))
    }
  }
  extract
}


expr_block <- function(exprs) {
  if (is.language(exprs)) {
    exprs <- list(exprs)
  } else {
    exprs <- flatten_eqs(exprs)
  }
  as.call(c(list(as.name("{")), exprs))
}


expr_if <- function(condition, a, b) {
  if (missing(b)) {
    call("if", condition, expr_block(a))
  } else {
    call("if", condition, expr_block(a), expr_block(b))
  }
}


expr_local <- function(exprs) {
  call("local", expr_block(exprs))
}


## ir_substitute and callees is used to rewrite expressions that use
## arrays within delay blocks
ir_substitute <- function(eqs, substitutions) {
  if (length(substitutions) == 0L) {
    return(eqs)
  }

  lapply(eqs, ir_substitute1, substitutions)
}


ir_substitute1 <- function(eq, substitutions) {
  from <- names(substitutions)
  if (any(from %in% eq$depends$variables)) {
    if (eq$type == "expression_array") {
      f <- function(x) {
        x$value <- ir_substitute_sexpr(x$value, substitutions)
        x
      }
      eq$rhs <- lapply(eq$rhs, f)
    } else {
      eq$rhs$value <- ir_substitute_sexpr(eq$rhs$value, substitutions)
    }
  }
  if (eq$lhs %in% from) {
    eq$lhs <- substitutions[[eq$lhs]]
  }
  eq
}


ir_substitute_sexpr <- function(expr, substitutions) {
  if (length(substitutions) == 0L) {
    expr
  } else if (is.recursive(expr)) {
    lapply(expr, ir_substitute_sexpr, substitutions)
  } else if (is.character(expr) && expr %in% names(substitutions)) {
    substitutions[[expr]]
  } else {
    expr
  }
}


support_coerce_mode <- function(value, integer, min, max, name) {
  if (integer) {
    if (!is_integer_like(value)) {
      stop(sprintf("Expected '%s' to be integer-like", name), call. = FALSE)
    }
    storage.mode(value) <- "integer"
  } else if (is.integer(value)) {
    storage.mode(value) <- "numeric"
  } else if (!is.numeric(value)) {
    stop(sprintf("Expected a numeric value for %s", name), call. = FALSE)
  }
  if (any(is.na(value))) {
    stop(sprintf("'%s' must not contain any NA values", name), call. = FALSE)
  }
  if (!is.null(min) && any(value < min)) {
    stop(sprintf("Expected '%s' to be at least %s", name, min),
         call. = FALSE)
  }
  if (!is.null(max) && any(value > max)) {
    stop(sprintf("Expected '%s' to be at most %s", name, max),
         call. = FALSE)
  }

  value
}
