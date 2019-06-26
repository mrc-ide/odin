## TODO: we should use getNativeSymbolInfo here for faster and more
## accurate lookup of symbols.  For now, all .Call statements need a
## PACKAGE argument.

## TODO: rework the whole use_dde bit

## TODO: all the core/ptr bits need work because we'll probably move
## these out to work directly with a native symbols where possible.

## TODO: The name here is out-of-sync with other naming conventions
## and should be thought about carefully.
odin_c_class <- function(base, core, user, features, dll, ir, package) {
  R6::R6Class(
    "odin_model",
    parent_env = environment(odin),
    cloneable = FALSE,
    private = list(
      ## Constant:
      name = base,
      core = core,
      dll = dll,
      discrete = features$discrete, # TODO: drop?
      user = user,
      ## Set at initialisation:
      ptr = NULL,
      use_dde = NULL,
      init = NULL,
      interpolate_t = NULL,
      ## Set more dynamically
      variable_order = NULL,
      output_order = NULL,
      ynames = NULL,
      n_out = NULL,
      ## Private method
      update_metadata = odin_c_class_update_metadata(features)
    ),
    public = drop_null(list(
      ir = if (package) c(dll, ir) else ir,
      initialize = odin_c_class_initialize(features),
      set_user = odin_c_class_set_user(features),
      initial = odin_c_class_initial(features),
      update = odin_c_class_update(features),
      deriv = odin_c_class_deriv(features),
      run = odin_c_class_run(features),
      contents = odin_c_class_contents(features),
      transform_variables = odin_c_class_transform(features))))
}


odin_c_class_set_user <- function(features, env = .GlobalEnv) {
  args <- alist("..." =, user = , unused_user_action = NULL)
  args[[2]] <- quote(list(...))

  check_user <- quote(
    support_check_user(
      user, private$user, unused_user_action))
  set_user_c <- call(".Call", quote(private$core$set_user), quote(private$ptr),
                     quote(user), PACKAGE = quote(private$dll))
  if (features$initial_time_dependent) {
    set_initial <- NULL
  } else {
    t0 <- if (features$discrete) NA_integer_ else NA_real_
    set_initial <- call(
      "<-", quote(private$init),
      call(".Call", quote(private$core$initial_conditions),
           quote(private$ptr), t0, PACKAGE = quote(private$dll)))
  }
  update_metadata <- quote(private$update_metadata())

  body <- list(check_user, set_user_c, set_initial, update_metadata)
  as_function(args, r_expr_block(body), env)
}


odin_c_class_initial <- function(features, env = .GlobalEnv) {
  time <- if (features$discrete) STEP else TIME
  args <- alist(time =)
  names(args) <- time
  if (features$initial_time_dependent) {
    time_clean <- if (features$discrete) "as_integer" else "as_numeric"
    body <- call(".Call", quote(private$core$initial_conditions),
                 quote(private$ptr), call(time_clean, as.name(time)),
                 PACKAGE = quote(private$dll))
  } else {
    body <- quote(private$init)
  }
  as_function(args, r_expr_block(body), env)
}


odin_c_class_update <- function(features, env = .GlobalEnv) {
  if (features$discrete) {
    args <- alist(step =, y =)
    if (features$has_delay) {
      body <- quote(stop("Can't call update() on delay models"))
    } else {
      body <- call(".Call", quote(private$core$rhs_r), quote(private$ptr),
                   quote(as_integer(step)), quote(as_numeric(y)),
                   PACKAGE = quote(private$dll))
    }
    as_function(args, r_expr_block(body), env)
  } else {
    NULL
  }
}


odin_c_class_deriv <- function(features, env = .GlobalEnv) {
  if (features$discrete) {
    NULL
  } else {
    args <- alist(t =, y =)
    body <- call(".Call", quote(private$core$rhs_r), quote(private$ptr),
                 quote(as_numeric(t)), quote(as_numeric(y)),
                 PACKAGE = quote(private$dll))
    as_function(args, r_expr_block(body), env)
  }
}


odin_c_class_run <- function(features, env = .GlobalEnv) {
  if (features$discrete) {
    odin_c_class_run_discrete(features, env)
  } else {
    odin_c_class_run_continuous(features, env)
  }
}


odin_c_class_run_continuous <- function(features, env = .GlobalEnv) {
  args <- alist(t =, y = NULL, "..." =, use_names = TRUE, tcrit = NULL)
  if (features$has_delay) {
    args <- c(args, alist(n_history = DEFAULT_HISTORY_SIZE))
  }

  check_t <- quote(t <- as_numeric(t))
  check_y1 <- r_expr_if(quote(!is.null(y)), quote(y <- as_numeric(y)))
  if (features$has_delay) {
    set_initial <- call(".Call", quote(private$core$set_initial),
                        quote(private$ptr), quote(t[[1]]), quote(y),
                        quote(private$use_dde), PACKAGE = quote(private$dll))
  } else {
    set_initial <- NULL
  }
  if (features$has_interpolate) {
    check_interpolate <- quote(
      tcrit <- support_check_interpolate_t(t, private$interpolate_t, tcrit))
  } else {
    check_interpolate <- NULL
  }
  check_y2 <- r_expr_if(quote(is.null(y)), quote(y <- self$initial(t[[1]])))

  args_dde <- list(
    quote(dde::dopri), quote(y), quote(t),
    quote(private$core$rhs_dde), quote(private$ptr),
    dllname = quote(private$dll), parms_are_real = FALSE,
    n_out = quote(private$n_out), output = quote(private$core$output),
    ynames = FALSE, tcrit = quote(tcrit), quote(...))
  args_ds <- list(
    quote(deSolve::ode), quote(y), quote(t),
    quote(private$core$rhs_desolve), quote(private$ptr),
    initfunc = quote(private$core$initmod_desolve),
    nout = quote(private$n_out), dllname = quote(private$dll),
    tcrit = quote(tcrit), quote(...))
  if (features$has_delay) {
    args_ds[[1]] <- quote(deSolve::dede)
    args_ds <- c(args_ds, list(control = quote(list(mxhist = n_history))))
    args_dde <- c(args_dde, list(n_history = quote(n_history)))
  }
  run <- r_expr_if(quote(private$use_dde),
                   list(call("<-", quote(ret), as.call(args_dde))),
                   list(call("<-", quote(ret), as.call(args_ds))))

  cleanup <- r_expr_if(
    quote(use_names),
    quote(colnames(ret) <- private$ynames),
    quote(colnames(ret) <- NULL))

  body <- drop_null(list(check_t, check_y1, set_initial, check_interpolate,
                         check_y2, run, cleanup, quote(ret)))
  as_function(args, r_expr_block(body), env)
}


odin_c_class_run_discrete <- function(features, env = .GlobalEnv) {
  args <- alist(step =, y = NULL, "..." =, use_names = TRUE, replicate = NULL)

  check_step <- quote(step <- as_integer(step))
  check_y <- r_expr_if(quote(is.null(y)), quote(y <- self$initial(step)))
  if (features$has_interpolate) {
    check_interpolate <-
      quote(support_check_interpolate_t(step, private$interpolate_t, NULL))
  } else {
    check_interpolate <- NULL
  }

  run_args <- list(quote(y), quote(step), quote(private$core$rhs_dde),
                   quote(private$ptr), dllname = quote(private$dll),
                   parms_are_real = FALSE, ynames = FALSE,
                   n_out = quote(private$n_out), quote(...))
  run_args1 <- c(list(quote(dde::difeq)), run_args)
  run_args2 <- c(list(quote(dde::difeq_replicate), quote(replicate)), run_args)
  run <- r_expr_if(quote(is.null(replicate)),
                   list(call("<-", quote(ret), as.call(run_args1))),
                   list(call("<-", quote(ret), as.call(run_args2))))
  cleanup <- r_expr_if(
    quote(use_names),
    quote(colnames(ret) <- private$ynames),
    quote(colnames(ret) <- NULL))

  body <- drop_null(list(check_step, check_y, check_interpolate, run,
                         cleanup, quote(ret)))
  as_function(args, r_expr_block(body), env)
}


odin_c_class_update_metadata <- function(features, env = .GlobalEnv) {
  body <- list(
    call("<-", quote(meta),
         call(".Call", quote(private$core$metadata), quote(private$ptr),
              PACKAGE = quote(private$dll))),
    quote(private$variable_order <- meta$variable_order),
    quote(private$output_order <- meta$output_order),
    quote(private$n_out <- meta$n_out),
    call("<-", quote(private$ynames),
         call("make_names", quote(private$variable_order),
              quote(private$output_order), features$discrete)),
    quote(private$interpolate_t <- meta$interpolate_t))
  as_function(list(), r_expr_block(body), env)
}


odin_c_class_initialize <- function(features, env = .GlobalEnv) {
  args <- alist(user = NULL, unused_user_action = NULL)
  if (features$discrete) {
    set_use_dde <- NULL
  } else {
    args <- c(args, list(use_dde = FALSE))
    set_use_dde <- quote(private$use_dde <- use_dde)
  }

  make_ptr <- as.call(list(
    as.name("<-"),
    quote(private$ptr),
    call(".Call", quote(private$core$create), PACKAGE = quote(private$dll))))
  body <- drop_null(list(
    set_use_dde,
    make_ptr,
    quote(self$set_user(user = user, unused_user_action = unused_user_action)),
    quote(lockBinding("ir", self))))
  as_function(args, r_expr_block(body), env)
}


odin_c_class_contents <- function(features, env = .GlobalEnv) {
  body <- call(".Call", quote(private$core$contents), quote(private$ptr),
               PACKAGE = quote(private$dll))
  as_function(alist(), r_expr_block(body), env)
}


odin_c_class_transform <- function(features, env = .GlobalEnv) {
  args <- alist(y =)
  body <- call("support_transform_variables", quote(y), quote(private))
  as_function(args, r_expr_block(body), env)
}
