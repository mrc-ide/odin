## These two are temporary!
odin2 <- function(x, validate = TRUE, verbose = TRUE) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin2_(xx, validate, verbose)
}


odin2_ <- function(x, validate = TRUE, verbose = TRUE) {
  ir <- odin_build_ir(x)
  odin_ir_generate(ir, validate)
}


## TODO: this needs a bunch of naming work - currently the prefix here
## is "odin_ir_generate" but this is "ir -> r" - we'll have "ir -> c"
## and eventually "ir -> js" here so we'll probably move to something
## like "gen_<target>_xxx" once this is working properly

odin_ir_generate <- function(ir, validate = TRUE) {
  if (validate) {
    ir_validate(ir, error = TRUE)
  }
  dat <- ir_deserialise(ir)
  dat$ir <- ir

  ## Pull this out into something generally useful
  features_supported <- c("has_user", "has_output", "discrete", "has_array",
                          "has_interpolate", "has_stochastic", "has_delay",
                          "initial_time_dependent")
  features_used <- vlapply(dat$features, identity)
  msg <- setdiff(names_if(features_used), features_supported)
  if (length(msg) > 0L) {
    stop("Features not suppored: ", paste(dquote(msg), collapse = ", "))
  }

  ## TODO: this is just punting for later
  ##
  ## NOTE: 'state' here is where *variables* are put.  This probably
  ## wants tightening up later...
  discrete <- dat$features$discrete
  meta <- list(
    internal = quote(INTERNAL),
    user = as.name(USER),
    state = as.name(STATE),
    result = if (discrete) as.name(STATE_NEXT) else as.name(DSTATEDT),
    output = as.name(OUTPUT),
    time = if (discrete) as.name(STEP) else as.name(TIME),
    initial_time = as.name(paste0("initial_", if (discrete) STEP else TIME)),
    use_dde = as.name(USE_DDE),
    get_user_double = as.name("_get_user_double"),
    get_user_dim = as.name("_get_user_dim"),
    check_interpolate_y = as.name("_check_interpolate_y"),
    check_interpolate_t = as.name("_check_interpolate_t"))

  ## This is our little rewriter - we'll tidy this up later
  rewrite <- function(x) {
    sexp_to_rexp(x, dat$data, meta)
  }

  eqs <- odin_ir_generate_expressions(dat, meta, rewrite)

  ## Then start putting together the initial conditions
  env <- new.env(parent = odin_base_env())

  ## Support functions will come in this way:
  if (dat$features$has_user) {
    env[[as.character(meta$get_user_double)]] <- support_get_user_double
    env[[as.character(meta$get_user_dim)]] <- support_get_user_dim
  }
  if (dat$features$has_interpolate) {
    env[[as.character(meta$check_interpolate_y)]] <- support_check_interpolate_y
    env[[as.character(meta$check_interpolate_t)]] <- support_check_interpolate_t
  }

  core <- odin_ir_generate_core(eqs, dat, env, meta, rewrite)
  odin_ir_generate_class(core, dat, env, meta)
}


odin_ir_generate_core <- function(eqs, dat, env, meta, rewrite) {
  core <- list(
    create = odin_ir_generate_create(eqs, dat, env, meta),
    ic = odin_ir_generate_ic(eqs, dat, env, meta, rewrite),
    set_user = odin_ir_generate_set_user(eqs, dat, env, meta),
    rhs_desolve = odin_ir_generate_rhs(eqs, dat, env, meta, rewrite, "desolve"),
    rhs_dde = odin_ir_generate_rhs(eqs, dat, env, meta, rewrite, "dde"),
    output = odin_ir_generate_rhs(eqs, dat, env, meta, rewrite,"output"),
    interpolate_t = odin_ir_generate_interpolate_t(dat, env, meta, rewrite),
    set_initial = odin_ir_generate_set_initial(dat, env, meta, rewrite),
    run = odin_ir_generate_run(dat, env, meta, rewrite),
    metadata = odin_ir_generate_metadata(dat, meta, rewrite))
  list2env(core, env)
  core
}


odin_ir_generate_create <- function(eqs, dat, env, meta) {
  alloc <- call("<-", meta[["internal"]], quote(new.env(parent = emptyenv())))
  eqs_create <- flatten_eqs(eqs[dat$components$create$equations])
  ret <- meta[["internal"]]
  body <- as.call(c(list(quote(`{`)), c(alloc, eqs_create, ret)))
  as_function(alist(), body, env)
}


## TODO: 'ic' ==> 'initial'
odin_ir_generate_ic <- function(eqs, dat, env, meta, rewrite) {
  ## Equations to run before initial conditions are computed:
  eqs_initial <- flatten_eqs(eqs[dat$components$initial$equations])

  ## We need a little fiction here because any use of a variable must
  ## use its "initial" name at this point.  We could filter through
  ## dependencies and work out if this is necessary, but this should
  ## be fairly harmless, and at the moment we don't report this well.
  subs <- lapply(dat$data$variable$contents, function(x) rewrite(x$initial))
  eqs_initial <- lapply(eqs_initial, substitute_, as.environment(subs))

  ## Allocate space for the state vector
  var_length <- rewrite(dat$data$variable$length)
  alloc <- call("<-", meta$state, call("numeric", var_length))

  ## Assign into the state vector
  f <- function(x) {
    d <- dat$data$data[[x$name]]
    if (d$rank == 0L) {
      target <- call("[[", meta$state, offset_to_position(x$offset))
    } else {
      offset <- rewrite(x$offset)
      seq <- call("seq_len", rewrite(d$dimnames$length))
      target <- call("[", meta$state, call("+", offset, seq))
    }
    call("<-", target, rewrite(x$initial))
  }
  assign <- lapply(dat$data$variable$contents, f)

  ## Build the function:
  body <- as.call(c(list(quote(`{`)), eqs_initial, alloc, assign, meta$state))
  args <- alist(time =, internal =)
  names(args)[[1]] <- as.character(meta$time)
  names(args)[[2]] <- as.character(meta$internal)
  as_function(args, body, env)
}


odin_ir_generate_set_user <- function(eqs, dat, env, meta) {
  eqs_user <- flatten_eqs(eqs[dat$components$user$equations])
  args <- alist(user =, internal =)
  names(args)[[1]] <- as.character(meta$user)
  names(args)[[2]] <- as.character(meta$internal)
  body <- as.call(c(list(quote(`{`)), eqs_user))
  as_function(args, body, env)
}


odin_ir_generate_rhs <- function(eqs, dat, env, meta, rewrite, rhs_type) {
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
    d <- dat$data$data[[x$name]]
    if (d$rank == 0L) {
      extract <- call("[[", meta$state, offset_to_position(x$offset))
    } else {
      seq <- call("seq_len", rewrite(d$dimnames$length))
      extract <- call("[", meta$state, call("+", x$offset, seq))
      if (d$rank > 1L) {
        extract <- call("array", extract, odin_ir_generate_dim(d, rewrite))
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
  alloc_result <- call("<-", meta$result,
                       call("numeric", call("length", meta$state)))

  ## For output_length we have no real choice but to look up the
  ## length each time.
  alloc_output <- call("<-", meta$output,
                       call("numeric", rewrite(dat$data$output$length)))
  alloc <- list(rhs = alloc_result, output = alloc_output)[use]

  if (rhs_type == "desolve") {
    if (has_output) {
      ret <- call("list", meta$result, meta$output)
    } else {
      ret <- call("list", meta$result)
    }
  } else if (rhs_type == "output") {
    ret <- meta$output
  } else {
    if (discrete && has_output) {
      ret <- list(
        call("<-", call("attr", meta$result, "output"), meta$output),
        meta$result)
    } else {
      ret <- meta$result
    }
  }

  eqs_include <- flatten_eqs(eqs[use_eqs])
  body <- as.call(c(list(quote(`{`)), c(vars, alloc, eqs_include, ret)))
  args <- alist(t = , y =, parms = )
  names(args)[[1]] <- as.character(meta$time)
  names(args)[[2]] <- as.character(meta$state)
  names(args)[[3]] <- as.character(meta$internal)
  as_function(args, body, env)
}


odin_ir_generate_metadata <- function(dat, meta, rewrite) {
  ord <- function(location) {
    contents <- dat$data$data[names(dat$data[[location]]$contents)]
    len <- lapply(contents, odin_ir_generate_dim, rewrite)
    as.call(c(list(quote(list)), len))
  }

  ynames <- call(
    "make_names2",
    quote(private$variable_order), quote(private$output_order),
    dat$features$discrete)
  n_out <- quote(support_n_out(private$output_order))

  env <- new.env(parent = environment(odin))

  body <- list(
    call("<-", meta$internal, quote(private$data)),
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


odin_ir_generate_interpolate_t <- function(dat, env, meta, rewrite) {
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

  args <- set_names(alist(internal = ), as.character(meta$internal))
  body <- call("{",
               call("list",
                    min = min, max = max, critical = critical))
  as_function(args, body, env)
}


odin_ir_generate_set_initial <- function(dat, env, meta, rewrite) {
  if (!dat$features$has_delay) {
    return(function(...) NULL)
  }

  set_y <- call(
    "if", call("!", call("is.null", meta$state)),
    expr_block(lapply(dat$data$variable$contents, function(x)
      call("<-", rewrite(x$initial),
           extract_variable(x, dat$data$data, meta$state, rewrite)))))
  set_t <- call("<-",
                rewrite(as.character(meta$initial_time)),
                meta$time)
  set_use_dde <- call("<-",
                      rewrite(as.character(meta$use_dde)),
                      meta$use_dde)
  body <- list(set_y, set_t, set_use_dde)
  args <- set_names(
    alist(, , , ),
    c(as.character(meta$time), as.character(meta$state),
      as.character(meta$use_dde), as.character(meta$internal)))

  as_function(args, expr_block(body), env)
}


## Thos feels pretty messy, but I think we can clean it up later.
## It's quite likely that we'd be better off with two separate
## generators - one for discrete and one for continuous models.
odin_ir_generate_run <- function(dat, env, meta, rewrite) {
  args <- alist(data =, t =, y = , n_out =, ynames =, ...=,
                  ## These are only used in some cases!
                  interpolate_t =)
  if (dat$features$discrete) {
    args <- c(args, alist(replicate =))
  } else {
    args <- c(args, alist(use_dde =, tcrit =))
  }
  if (dat$features$has_delay) {
    args <- c(args, alist(n_history = 1000L))
  }

  names(args)[[1L]] <- as.character(meta$internal)
  names(args)[[2L]] <- as.character(meta$time)
  names(args)[[3L]] <- as.character(meta$state)
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

  t0 <- call("[[", meta$time, 1L)

  if (dat$features$has_delay) {
    set_initial <- call("set_initial", t0, meta$state, use_dde, meta$internal)
  } else {
    set_initial <- NULL
  }

  if (dat$features$has_interpolate) {
    check_interpolate_t <-
      call(as.character(meta$check_interpolate_t),
           meta$time, interpolate_t, tcrit)
    if (!dat$features$discrete) {
      check_interpolate_t <- call("<-", tcrit, check_interpolate_t)
    }
  } else {
    check_interpolate_t <- NULL
  }

  compute_initial <-
    call("if", call("is.null", meta$state),
         expr_block(call("<-", meta$state, call("ic", t0, meta$internal))))

  output <- if (dat$features$has_output) quote(output) else NULL

  run_args_dde <- list(meta$state, meta$time, quote(rhs_dde),
                       meta$internal, ynames = FALSE, quote(`...`))
  run_args_desolve <- list(meta$state, meta$time, quote(rhs_desolve),
                           meta$internal, quote(`...`))
  if (!dat$features$discrete) {
    run_args_desolve <- c(run_args_desolve, tcrit = tcrit)
  }
  if (dat$features$has_output) {
    run_args_dde <- c(run_args_dde, list(n_out = n_out))
    if (!dat$features$discrete) {
      run_args_dde <- c(run_args_dde, list(output = quote(output)))
    }
  }
  if (dat$features$has_delay) {
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


## TODO: this should be generate_equation I think
odin_ir_generate_expression <- function(eq, dat, meta, rewrite) {
  f <- switch(
    eq$type,
    alloc = odin_ir_generate_expression_alloc,
    alloc_interpolate = odin_ir_generate_expression_alloc_interpolate,
    copy = odin_ir_generate_expression_copy,
    user = odin_ir_generate_expression_user,
    expression_scalar = odin_ir_generate_expression_scalar,
    expression_array = odin_ir_generate_expression_array,
    delay_index = odin_ir_generate_expression_delay_index,
    delay_continuous = odin_ir_generate_expression_delay_continuous,
    stop("Unknown type"))

  data_info <- dat$data$data[[eq$lhs$target]]
  stopifnot(!is.null(data_info))

  f(eq, data_info, dat, meta, rewrite)
}


odin_ir_generate_expressions <- function(dat, meta, rewrite) {
  lapply(dat$equations, odin_ir_generate_expression, dat, meta, rewrite)
}


sexp_to_rexp <- function(x, data, meta) {
  if (is.recursive(x)) {
    fn <- x[[1L]]
    args <- x[-1L]
    if (fn == "length") {
      sexp_to_rexp(data$data[[args[[1L]]]]$dimnames$length, data, meta)
    } else if (fn == "dim") {
      nm <- data$data[[args[[1L]]]]$dimnames$dim[[args[[2L]]]]
      sexp_to_rexp(nm, data, meta)
    } else if (fn == "interpolate") {
      as.call(list(sexp_to_rexp(args[[1L]], data, meta), meta$time))
    } else if (fn == "odin_sum") {
      sexp_to_rexp_sum(lapply(args, sexp_to_rexp, data, meta))
    } else if (fn == "norm_rand") {
      quote(rnorm(1L))
    } else {
      as.call(c(list(as.name(fn)), lapply(args, sexp_to_rexp, data, meta)))
    }
  } else if (is.character(x)) {
    location <- data$data[[x]]$location
    if (!is.null(location) && location == "internal") {
      call("[[", meta$internal, x)
    } else {
      as.name(x)
    }
  } else if (is.integer(x)) {
    as.numeric(x)
  } else {
    x
  }
}


sexp_to_rexp_sum <- function(args) {
  f <- function(a, b) {
    if (identical(a, b)) a else call("seq.int", a, b, by = 1L)
  }
  i <- seq(2L, by = 2L, to = length(args))
  idx <- Map(f, args[i], args[i + 1L])
  call("sum", as.call(c(list(as.name("["), args[[1L]]), idx)))
}


odin_ir_generate_expression_scalar <- function(eq, data_info, dat, meta,
                                               rewrite) {
  location <- data_info$location

  if (location == "internal" || location == "transient") {
    lhs <- rewrite(eq$name)
  } else {
    offset <- dat$data[[location]]$contents[[data_info$name]]$offset
    storage <- if (location == "variable") meta$result else meta$output
    lhs <- call("[[", storage, offset_to_position(offset))
  }

  rhs <- rewrite(eq$rhs$value)
  call("<-", lhs, rhs)
}


odin_ir_generate_expression_array <- function(eq, data_info, dat, meta,
                                              rewrite) {
  lhs <- odin_ir_generate_expression_array_lhs(
    eq, data_info, dat$data, meta, rewrite)
  lapply(eq$rhs, odin_ir_generate_expression_array_rhs,
         lhs, data_info, dat$data, meta, rewrite)
}


## For internal storage we can do:
##   STORAGE[[NAME]][i, j]
## but the variable/output case is different as it's
##   STORAGE[OFFSET + f(i, j)]
##
## In C we'll do this as
##   STORAGE->NAME[f(i, j)]
## and
##   STORAGE[OFFSET + f(i, j)]
odin_ir_generate_expression_array_lhs <- function(eq, data_info, data, meta,
                                                  rewrite) {
  ## All the rhs have the same structure so we can use any of them
  ## here - we need only to get the index element out
  index <- lapply(eq$rhs[[1]]$index, function(x) as.name(x$index))
  location <- data_info$location

  if (location == "internal") {
    lhs <- as.call(c(list(quote(`[`), rewrite(data_info$name)), index))
  } else {
    f <- function(i) {
      if (i == 1) {
        index[[i]]
      } else {
        call("*", rewrite(data_info$dimnames$mult[[i]]),
             call("-", index[[i]], 1L))
      }
    }
    pos <- collapse_expr(lapply(seq_len(data_info$rank), f), "+")
    offset <- rewrite(data[[location]]$contents[[data_info$name]]$offset)
    storage <- if (location == "variable") meta$result else meta$output
    lhs <- call("[[", storage, call("+", offset, pos))
  }

  lhs
}


odin_ir_generate_expression_array_rhs <- function(rhs, lhs, data_info,
                                                  data, meta, rewrite) {
  ret <- call("<-", lhs, rewrite(rhs$value))
  subs <- list()
  for (idx in rev(rhs$index)) {
    value <- rewrite(idx$value)
    if (idx$is_range) {
      ret <- call("for", as.name(idx$index), value, call("{", ret))
    } else {
      subs[[idx$index]] <- value
    }
  }
  if (length(subs) > 0L) {
    ret <- substitute_(ret, subs)
  }
  ret
}


odin_ir_generate_expression_array_rhs2 <- function(value, index, lhs, rewrite) {
  ret <- call("<-", lhs, rewrite(value))
  subs <- list()
  for (idx in rev(index)) {
    value <- rewrite(idx$value)
    if (idx$is_range) {
      ret <- call("for", as.name(idx$index), value, call("{", ret))
    } else {
      subs[[idx$index]] <- value
    }
  }
  if (length(subs) > 0L) {
    ret <- substitute_(ret, subs)
  }
  ret
}


odin_ir_generate_expression_alloc <- function(eq, data_info, dat, meta,
                                              rewrite) {
  lhs <- rewrite(eq$lhs$target)
  alloc_fn <- switch(data_info$storage_type,
                     double = "numeric",
                     int = "integer",
                     stop(sprintf("unsupported storage type")))
  len <- rewrite(data_info$dimnames$length)
  rhs <- call(alloc_fn, len)
  if (data_info$rank > 1L) {
    rhs <- call("array", rhs, odin_ir_generate_dim(data_info, rewrite))
  }
  call("<-", lhs, rhs)
}


odin_ir_generate_expression_alloc_interpolate <- function(eq, data_info,
                                                          dat, meta,
                                                          rewrite) {
  name_target <- eq$lhs$target
  name_arg <- eq$interpolate$y

  data <- dat$data
  data_info_target <- data$data[[name_target]]
  data_info_t <- data$data[[eq$interpolate$t]]
  data_info_arg <- data$data[[eq$interpolate$y]]

  dim_arg <- odin_ir_generate_dim(data_info_arg, rewrite)

  len_t <- rewrite(data_info_t$dimnames$length)
  if (data_info$rank == 0L) {
    dim_target <- len_t
  } else {
    if (data_info_target$rank == 1L) {
      dim_target <- rewrite(data_info_target$dimnames$length)
    } else {
      dim_target <- lapply(data_info_target$dimnames$dim, rewrite)
    }
    dim_target <- as.call(c(quote(c), len_t, dim_target))
  }

  check <- call(as.character(meta$check_interpolate_y),
                dim_arg, dim_target, name_arg, name_target)

  lhs <- rewrite(eq$name)
  args <- list(quote(cinterpolate::interpolation_function),
               rewrite(eq$interpolate$t),
               rewrite(eq$interpolate$y),
               eq$interpolate$type,
               scalar = TRUE)
  rhs <- as.call(args)
  list(check, call("<-", lhs, rhs))
}


odin_ir_generate_expression_copy <- function(eq, data_info, dat, meta,
                                             rewrite) {
  ## NOTE: this applies only to copying a variable into the output
  offset <- rewrite(dat$data$output$contents[[eq$lhs$target]]$offset)
  storage <- meta$output

  if (data_info$rank == 0) {
    lhs <- call("[[", storage, offset_to_position(offset))
  } else{
    i <- call("seq_len", rewrite(data_info$dimnames$length))
    lhs <- call("[", storage, call("+", offset, i))
  }

  rhs <- rewrite(eq$lhs$target)
  call("<-", lhs, rhs)
}


## NOTE: There are two entirely separate codepaths here so this could
## be factored out again (and probably should be).
odin_ir_generate_expression_user <- function(eq, data_info, dat, meta,
                                             rewrite) {
  if (eq$user$dim) {
    name <- eq$name
    len <- data_info$dimnames$length
    if (data_info$rank == 1L) {
      dims <- NULL
    } else {
      ## NOTE: passing *names* in, not rewritten expressions
      dims <- as.call(c(list(quote(c)), data_info$dimnames$dim))
    }
    call(as.character(meta$get_user_dim), meta$user, meta$internal,
         name, len, dims)
  } else {
    lhs <- rewrite(eq$name)
    rank <- data_info$rank
    if (is.null(eq$user$default)) {
      default <- NULL
    } else {
      default <- rewrite(eq$user$default)
    }
    size <- odin_ir_generate_dim(data_info, rewrite)
    rhs <- call(as.character(meta$get_user_double),
                meta$user, eq$name, meta$internal, size, default)
    call("<-", lhs, rhs)
  }
}


odin_ir_generate_expression_delay_index <- function(eq, data_info, dat, meta,
                                                    rewrite) {
  d <- dat$delay[[eq$delay]]
  target <- rewrite(eq$name)
  alloc <- call("<-", target, call("integer", rewrite(d$variables$length)))

  index1 <- function(v) {
    d <- dat$data$data[[v$name]]
    offset <- dat$data$variable$contents[[v$name]]$offset
    if (d$rank == 0L) {
      call("<-",
           call("[[", target, offset_to_position(v$offset)),
           offset_to_position(offset))
    } else {
      seq <- call("seq_len", rewrite(d$dimnames$length))
      call("<-",
           call("[", target, call("+", rewrite(v$offset), seq)),
           call("+", rewrite(offset), seq))
    }
  }

  index <- unname(lapply(d$variables$contents, index1))
  c(alloc, index)
}


odin_ir_generate_expression_delay_continuous <- function(eq, data_info,
                                                         dat, meta, rewrite) {
  d <- dat$delay[[eq$lhs$target]]

  tau <- rewrite(eq$rhs$time)
  initial_time <- rewrite(as.character(meta$initial_time))
  state <- rewrite(d$state)
  index <- rewrite(d$index)

  ## TODO: If we have an array then we need to have the expressions
  ## rewritten.  I think that the easiest way of doing that is in the
  ## ir where we just go through and rewrite the whole lot, really.
  ## But then it's done in only one place and will generally work.  We
  ## just end up with even more equations added, but they can be put
  ## right after the canonical version (or just at the end of the
  ## block really).  The problem is it sort of breaks the graph and
  ## adds a lot of junk.
  time <- call("<-", meta$time, call("-", meta$time, tau))

  lookup_vars <- expr_if(
    rewrite(as.character(meta$use_dde)),
    call("<-", state, as.call(c(quote(dde::ylag), meta$time, index))),
    call("<-", state, as.call(c(quote(deSolve::lagvalue), meta$time, index))))
  unpack_vars <- lapply(d$variables$contents,
                        unpack_variable, dat$data$data, state, rewrite)

  eqs <- flatten_eqs(lapply(dat$equations[d$equations],
                            odin_ir_generate_expression, dat, meta, rewrite))

  ## Only used where there is no default:
  unpack_initial <-
    lapply(dat$data$variable$contents[names(d$variables$contents)],
           function(x) call("<-", as.name(x$name), rewrite(x$initial)))
  unpack <- expr_if(call("<=", meta$time, initial_time),
                    unpack_initial, c(lookup_vars, unpack_vars))

  if (data_info$rank == 0L) {
    rhs <- rewrite(eq$rhs$value)
    if (is.null(eq$rhs$default)) {
      body <- expr_local(c(time, unpack, eqs, rhs))
      ret <- call("<-", rewrite(eq$name), body)
    } else {
      default <- rewrite(eq$rhs$default)
      body <- expr_local(list(
        time,
        expr_if(
          call("<=", meta$time, initial_time),
          default,
          c(lookup_vars, unpack_vars, eqs, rhs))))
      ret <- call("<-", rewrite(eq$name), body)
    }
  } else {
    ## TODO: generating the lhs by hand because
    ## 'odin_ir_generate_expression_array_lhs' assumes things about
    ## expressions that are not correct here.
    index <- lapply(eq$rhs$index, function(x) as.name(x$index))
    lhs <- as.call(c(list(quote(`[`), rewrite(data_info$name)), index))
    expr <- odin_ir_generate_expression_array_rhs2(
      eq$rhs$value, eq$rhs$index, lhs, rewrite)
    if (is.null(eq$rhs$default)) {
      ret <- expr_local(c(time, unpack, eqs, expr))
    } else {
      default <- odin_ir_generate_expression_array_rhs2(
        eq$rhs$default, eq$rhs$index, lhs, rewrite)
      ret <- expr_local(list(
        time,
        expr_if(
          call("<=", meta$time, initial_time),
          default,
          c(lookup_vars, unpack_vars, eqs, expr))))
    }
  }

  ret
}


odin_ir_generate_dim <- function(data_info, rewrite) {
  if (data_info$rank == 0L) {
    NULL
  } else if (data_info$rank == 1L) {
    rewrite(data_info$dimnames$length)
  } else {
    as.call(c(list(quote(c)), lapply(data_info$dimnames$dim, rewrite)))
  }
}


offset_to_position <- function(x) {
  if (is.language(x)) {
    call("+", x, 1L)
  } else {
    x + 1L
  }
}


## There's a really big question here about whether odin is going to
## generate R *code* or *R objects* here.  For now I am generating
## objects and we'll come back and generate code later on.  The latter
## is needed for generating package code for example.
##
## TODO: The other way of doing this, which might be nicer, is for a
## top-level class and then inject dependencies into it.  We should be
## able to do this with minimal overhead and just as much of the class
## locked down to creation time.
odin_ir_generate_class <- function(core, dat, env, meta) {
  self <- private <- NULL # quieten global check: R6 adds these later
  if (dat$features$has_interpolate) {
    loadNamespace("cinterpolate")
  }


  ## This bit needs thinking about carefully - there are two
  ## user-focussed changes here that change between
  ## discrete/nondiscrete.  This is unfortunately affected by the
  ## delays too because if there are delays then we need to keep track
  ## of history size.
  if (dat$features$discrete) {
    run <- function(step, y = NULL, ..., use_names = TRUE, replicate = NULL) {
      private$core$run(private$data, step, y, private$n_out,
                       if (use_names) private$ynames else NULL,
                       ...,
                       replicate = replicate,
                       interpolate_t = private$interpolate_t)
    }
  } else {
    run <- function(t, y = NULL, ..., use_names = TRUE, tcrit = NULL) {
      private$core$run(private$data, t, y, private$n_out,
                       if (use_names) private$ynames else NULL,
                       tcrit = tcrit, ...,
                       use_dde = private$use_dde,
                       interpolate_t = private$interpolate_t)
    }
  }

  env[[dat$config$base]] <- R6::R6Class(
    ## TODO: use of 'odin_model' here is somewhat incorrect because
    ## the objects are not really substituable within a class.  This
    ## should probably come from the base name
    "odin_model",
    parent_env = environment(odin),
    cloneable = FALSE,

    private = list(
      name = dat$config$base,
      core = core,
      data = NULL,
      use_dde = NULL,
      init = NULL,
      initial_time_dependent = dat$features$initial_time_dependent,
      interpolate = dat$features$has_interpolate,
      interpolate_t = NULL,
      delay = dat$features$has_delay,
      ## TODO: this is a horrible name
      ir_ = dat$ir,

      ## These are not obviously the right bit of metadata to keep
      ## All of these might want better names.
      discrete = dat$features$discrete,
      variable_order = NULL,
      output_order = NULL,
      ynames = NULL,
      n_out = NULL
    ),

    public = drop_null(list(
      ## Methods:
      initialize = function(user = NULL, use_dde = FALSE) {
        ## TODO: why is 'use_dde' here in the initialiser and not in
        ## the run function?  We could take this as a default?
        ## Nothing looks like that would would be impossible.  Most
        ## likely we will need to support a period of the argument
        ## going here as well, with deprecation.
        private$use_dde <- use_dde || private$discrete
        if (private$use_dde) {
          loadNamespace("dde")
        }

        private$data <- private$core$create()
        self$set_user(user = user)
      },

      set_user = function(..., user = list(...)) {
        private$core$set_user(user, private$data)
        if (!private$initial_time_dependent) {
          private$init <- private$core$ic(NA_real_, private$data)
        }
        private$core$metadata(self, private)
      },

      update = if (dat$features$discrete) {
        function(step, y) {
          if (private$delay) {
            stop("Can't call deriv() on delay models")
          }
          private$core$rhs_dde(step, y, private$data)
        }
      },

      deriv = if (!dat$features$discrete) {
        function(t, y) {
          if (private$delay) {
            stop("Can't call deriv() on delay models")
          }
          ret <- private$core$rhs_dde(t, y, private$data)
          if (!is.null(private$core$output)) {
            attr(ret, "output") <- private$core$output(t, y, private$data)
          }
          ret
        }
      },

      ## TODO: This condition is actually constant for this class, so
      ## the logic at runtime makes very little sense.  It's here at
      ## the moment to keep things simple, and with a code-generation
      ## approach this could be replaced.
      initial = function(t) {
        if (private$initial_time_dependent) {
          private$core$ic(t, private$data)
        } else {
          private$init
        }
      },

      run = run,

      ## TODO: I am currently not sure if this belongs here or with
      ## the generator...
      ir = function() {
        private$ir_
      },

      contents = function() {
        sort_list(as.list(private$data))
      },

      transform_variables = function(y) {
        support_transform_variables(
          y, private$variable_order, private$output_order, private$discrete)
      }
    )))

  cl_init <- call("$", as.name(dat$config$base), quote(new))
  ## Need to build a nice argument list here.  This is pretty ugly and
  ## will be somewhat duplicated with different interfaces.
  if (dat$features$has_user) {
    i <- set_names(vlapply(dat$user, "[[", "has_default"),
                   vcapply(dat$user, "[[", "name"))
    nms <- names(i)
    args <- c(rep(alist(a = ), sum(!i)), rep(alist(a = NULL), sum(i)))
    names(args) <- nms
    args[[meta$user]] <-
      as.call(c(list(quote(list)),
                set_names(lapply(nms, as.name), nms)))
    args <- c(args, alist(use_dde = FALSE))
    body <- call("{", as.call(list(cl_init, meta$user, quote(use_dde))))
  } else {
    args <- alist(use_dde = FALSE)
    body <- call("{", as.call(list(cl_init, NULL, quote(use_dde))))
  }

  as_function(args, body, env)
}


## Some support functions - these are not subject to code generation
## at all and will be injected into the appropriate environment.
support_get_user_double <- function(user, name, internal, size, default) {
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

    if (is.integer(value)) {
      storage.mode(value) <- "numeric"
    } else if (!is.numeric(value)) {
      stop(sprintf("Expected a numeric value for %s", name), call. = FALSE)
    }
    if (any(is.na(value))) {
      stop(sprintf("'%s' must not contain any NA values", name), call. = FALSE)
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
support_get_user_dim <- function(user, internal, name, len, dims) {
  data <- user[[name]] %||% internal[[name]]
  if (is.null(data)) {
    stop(sprintf("Expected a value for '%s'", name), call. = FALSE)
  }
  if (is.integer(data)) {
    storage.mode(data) <- "numeric"
  } else if (!is.numeric(data)) {
    stop(sprintf("Expected a numeric value for %s", name), call. = FALSE)
  }
  if (any(is.na(data))) {
    stop(sprintf("'%s' must not contain any NA values", name), call. = FALSE)
  }
  d <- dim(data)
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
  internal[[len]] <- length(data)
  internal[[name]] <- data
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
  imports <- c("rnorm")
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
    extract <- call("[", state, call("+", x$offset, seq))
    if (d$rank > 1L) {
      extract <- call("array", extract, odin_ir_generate_dim(d, rewrite))
    }
  }
  extract
}


expr_block <- function(exprs) {
  as.call(c(list(as.name("{")), flatten_eqs(exprs)))
}


expr_if <- function(condition, a, b) {
    call("if", condition, expr_block(a), expr_block(b))
}


expr_local <- function(exprs) {
  call("local", expr_block(exprs))
}
