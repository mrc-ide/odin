## TODO: we should use getNativeSymbolInfo here for faster and more
## accurate lookup of symbols.  For now, all .Call statements need a
## PACKAGE argument.
generate_c_class <- function(core, dll, dat) {
  self <- private <- NULL # quieten global check: R6 adds these later
  if (dat$features$has_interpolate) {
    loadNamespace("cinterpolate")
  }

  ## TODO: This is unacceptably complicated
  if (dat$features$initial_time_dependent) {
    set_user <- function(..., user = list(...)) {
      .Call(private$core$set_user, private$ptr, user, PACKAGE = private$dll)
      private$update_metadata()
    }
    if (dat$features$discrete) {
      initial <- function(step) {
        .Call(private$core$initial_conditions, private$ptr, as_integer(step),
              PACKAGE = private$dll)
      }
    } else {
      initial <- function(t) {
        .Call(private$core$initial_conditions, private$ptr, as_numeric(t),
              PACKAGE = private$dll)
      }
    }
  } else {
    set_user <- function(..., user = list(...)) {
      .Call(private$core$set_user, private$ptr, user, PACKAGE = private$dll)
      private$init <-
        .Call(private$core$initial_conditions, private$ptr, NA_real_,
              PACKAGE = private$dll)
      private$update_metadata()
    }
    if (dat$features$discrete) {
      initial <- function(step) {
        private$init
      }
    } else {
      initial <- function(t) {
        private$init
      }
    }
  }

  ## TODO: it's possible that code generation here would help, as
  ## would a private function that does the middle bit alone?
  if (dat$features$discrete) {
    deriv <- NULL
    if (dat$features$has_delay) {
      update <- function(step, y) {
        stop("Can't call update() on delay models")
      }
    } else {
      update <- function(step, y) {
        .Call(private$core$rhs_r, private$ptr, as_integer(step), as_numeric(y),
              PACKAGE = private$dll)
      }
    }
    ## TODO: I don't see that this does the initial setting correctly
    ## - surely this should end up here.  This suggests that we do not
    ## have a case that uses the initial conditions correctly.
    run <- function(step, y = NULL, ..., use_names = TRUE, replicate = NULL) {
      step <- as_integer(step)
      if (is.null(y)) {
        y <- self$initial(step)
      }
      if (!is.null(private$interpolate_t)) {
        support_check_interpolate_t(step, private$interpolate_t, NULL)
      }
      if (is.null(replicate)) {
        ret <- dde::difeq(y, step, private$core$rhs_dde, private$ptr,
                          dllname = private$dll, parms_are_real = FALSE,
                          ynames = FALSE, n_out = private$n_out, ...)
      } else {
        ret <- dde::difeq_replicate(replicate, y, step,
                                    private$core$rhs_dde, private$ptr,
                                    dllname = private$dll,
                                    parms_are_real = FALSE,
                                    ynames = FALSE, n_out = private$n_out,
                                    ...)
      }
      if (use_names) {
        colnames(ret) <- private$ynames
      } else {
        colnames(ret) <- NULL
      }
      ret
    }
  } else if (dat$features$has_delay) {
    update <- NULL
    deriv <- function(t, y) {
      stop("Can't call deriv() on delay models")
    }

    run <- function(t, y = NULL, ..., use_names = TRUE, tcrit = NULL,
                    n_history = 1000L) {
      if (!is.null(y)) {
        y <- as_numeric(y)
      }
      .Call(private$core$set_initial, private$ptr, as_numeric(t[[1]]),
            y, private$use_dde, PACKAGE = private$dll)
      if (!is.null(private$interpolate_t)) {
        tcrit <- support_check_interpolate_t(t, private$interpolate_t, tcrit)
      }
      if (is.null(y)) {
        y <- self$initial(t)
      }
      if (private$use_dde) {
        ret <- dde::dopri(y, t, private$core$rhs_dde, private$ptr,
                          dllname = private$dll, parms_are_real = FALSE,
                          n_history = n_history, n_out = private$n_out,
                          output = private$core$output, ynames = FALSE,
                          tcrit = tcrit, ...)
      } else {
        ## TODO: initmod => initfunc
        ret <- deSolve::dede(y, t, private$core$rhs_desolve, private$ptr,
                             initfunc = private$core$initmod_desolve,
                             nout = private$n_out, dllname = private$dll,
                             control = list(mxhist = n_history),
                             tcrit = tcrit, ...)
      }
      if (use_names) {
        colnames(ret) <- private$ynames
      } else {
        colnames(ret) <- NULL
      }
      ret
    }
  } else {
    deriv <- function(t, y) {
      .Call(private$core$rhs_r, private$ptr, t, y, PACKAGE = private$dll)
    }

    run <- function(t, y = NULL, ..., use_names = TRUE, tcrit = NULL) {
      if (!is.null(private$interpolate_t)) {
        tcrit <- support_check_interpolate_t(t, private$interpolate_t, tcrit)
      }
      if (is.null(y)) {
        y <- self$initial(t)
      }
      if (private$use_dde) {
        ret <- dde::dopri(y, t, private$core$rhs_dde, private$ptr,
                          dllname = private$dll, parms_are_real = FALSE,
                          n_out = private$n_out,
                          output = private$core$output, ynames = FALSE,
                          tcrit = tcrit, ...)
      } else {
        ## TODO: initmod => initfunc
        ret <- deSolve::ode(y, t, private$core$rhs_desolve, private$ptr,
                            initfunc = private$core$initmod_desolve,
                            nout = private$n_out, dllname = private$dll,
                            tcrit = tcrit, ...)
      }
      if (use_names) {
        colnames(ret) <- private$ynames
      } else {
        colnames(ret) <- NULL
      }
      ret
    }
  }

  env <- new.env(parent = as.environment("package:base"))
  env[[dat$config$base]] <- R6::R6Class(
    "odin_model",
    parent_env = environment(odin),
    cloneable = FALSE,
    private = list(
      name = dat$config$base,

      core = core,
      ptr = NULL,
      dll = dll,

      use_dde = NULL,
      init = NULL,
      interpolate_t = NULL,
      ir_ = dat$ir,

      ## These are not obviously the right bit of metadata to keep
      ## All of these might want better names.
      discrete = dat$features$discrete,
      variable_order = NULL,
      output_order = NULL,
      ynames = NULL,
      n_out = NULL,

      update_metadata = function() {
        meta <- .Call(private$core$metadata, private$ptr, PACKAGE = private$dll)
        private$variable_order <- meta$variable_order
        private$output_order <- meta$output_order
        private$n_out <- meta$n_out
        private$ynames <- make_names2(private$variable_order,
                                      private$output_order,
                                      private$discrete)
        private$interpolate_t <- meta$interpolate_t
      }
    ),

    public = drop_null(list(
      initialize = function(user = NULL, use_dde = FALSE) {
        private$use_dde <- use_dde || private$discrete
        if (private$use_dde) {
          loadNamespace("dde")
        }
        private$ptr <- .Call(private$core$create, PACKAGE = private$dll)
        self$set_user(user = user)
      },

      initial = initial,
      set_user = set_user,
      run = run,
      deriv = deriv,
      update = update,

      ir = function() {
        private$ir_
      },

      contents = function() {
        .Call(private$core$contents, private$ptr, PACKAGE = private$dll)
      },

      transform_variables = function(y) {
        support_transform_variables(y, private)
      }
    )))

  generate_r_constructor(dat, env)
}
