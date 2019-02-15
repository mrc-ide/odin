generate_c_class <- function(core, dll, dat) {
  self <- private <- NULL # quieten global check: R6 adds these later
  if (dat$features$has_interpolate) {
    loadNamespace("cinterpolate")
  }

  if (dat$features$initial_time_dependent) {
    set_user <- function(..., user = list(...)) {
      .Call(private$core$set_user, private$ptr, user)
      private$update_metadata()
    }
    initial <- function(t) {
      .Call(private$core$initial_conditions, private$ptr, t)
    }
  } else {
    set_user <- function(..., user = list(...)) {
      .Call(private$core$set_user, private$ptr, user)
      private$init <-
        .Call(private$core$initial_conditions, private$ptr, NA_real_)
      private$update_metadata()
    }
    initial <- function(t) {
      private$init
    }
  }

  run <- function(t, y = NULL, ..., use_names = TRUE, tcrit = NULL) {
    if (is.null(y)) {
      y <- self$initial(t)
    }
    if (private$use_dde) {
      ret <- dde::dopri(y, t, private$core$rhs_dde, private$ptr,
                        dllname = private$dll, parms_are_real = FALSE,
                        ynames = FALSE, ...)
    } else {
      ## TODO: initmod => initfunc
      ret <- deSolve::ode(y, t, private$core$rhs_desolve, private$ptr,
                          initfunc = private$core$initmod_desolve,
                          dllname = private$dll, ...)
    }
    if (use_names) {
      colnames(ret) <- private$ynames
    } else {
      colnames(ret) <- NULL
    }
    ret
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
      delay = dat$features$has_delay,
      ir_ = dat$ir,

      ## These are not obviously the right bit of metadata to keep
      ## All of these might want better names.
      discrete = dat$features$discrete,
      variable_order = NULL,
      output_order = NULL,
      ynames = NULL,
      n_out = NULL,

      update_metadata = function() {
        meta <- .Call(private$core$metadata, private$ptr)
        private$variable_order <- meta$variable_order
        private$output_order <- meta$output_order
        private$n_out <- meta$n_out
        private$ynames <- make_names2(private$variable_order,
                                      private$output_order,
                                      private$discrete)
      }
    ),

    public = list(
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

      deriv = function(t, y) {
        if (private$delay) {
          stop("Can't call deriv() on delay models")
        }
        .Call(private$core$rhs_r, private$ptr, t, y, PACKAGE = private$dll)
      },

      ir = function() {
        private$ir_
      },

      contents = function() {
        .Call(private$core$contents, private$ptr, PACKAGE = private$dll)
      },

      transform_variables = function(y) {
        support_transform_variables(y, private)
      }
    ))

  generate_r_constructor(dat, env)
}
