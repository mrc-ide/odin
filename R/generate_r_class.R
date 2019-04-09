## There's a really big question here about whether odin is going to
## generate R *code* or *R objects* here.  For now I am generating
## objects and we'll come back and generate code later on.  The latter
## is needed for generating package code for example.
##
## TODO: The other way of doing this, which might be nicer, is for a
## top-level class and then inject dependencies into it.  We should be
## able to do this with minimal overhead and just as much of the class
## locked down to creation time.
generate_r_class <- function(core, dat, env) {
  self <- private <- NULL # quieten global check: R6 adds these later
  if (dat$features$has_interpolate) {
    loadNamespace("cinterpolate")
  }

  if (dat$features$discrete) {
    args <- alist(step =, y = NULL, "..." =, use_names = TRUE,
                  replicate = NULL)
    call <- list(quote(private$core$run), quote(private$data),
                 quote(step), quote(y), quote(private$n_out),
                 quote(if (use_names) private$ynames else NULL),
                 quote(...),
                 replicate = quote(replicate),
                 interpolate_t = quote(private$interpolate_t))
  } else {
    args <- alist(t =, y = NULL, "..." =, use_names = TRUE, tcrit = NULL)
    call <- list(quote(private$core$run), quote(private$data),
                 quote(t), quote(y), quote(private$n_out),
                 quote(if (use_names) private$ynames else NULL),
                 tcrit = quote(tcrit), quote(...),
                 use_dde = quote(private$use_dde),
                 interpolate_t = quote(private$interpolate_t))
  }
  run <- as.function(c(args, list(call("{", as.call(call)))), .GlobalEnv)

  if (dat$features$initial_time_dependent) {
    set_user <- function(..., user = list(...), unused_user_action = NULL) {
      private$core$set_user(user, private$data, unused_user_action)
      private$core$metadata(self, private)
    }
    ## TODO: we need to have 't' become 'step' for discrete time models
    initial <- function(t) {
      private$core$ic(t, private$data)
    }
  } else {
    set_user <- function(..., user = list(...), unused_user_action = NULL) {
      private$core$set_user(user, private$data, unused_user_action)
      private$init <- private$core$ic(NA_real_, private$data)
      private$core$metadata(self, private)
    }
    initial <- function(t) {
      private$init
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
      interpolate_t = NULL,
      delay = dat$features$has_delay,

      ## These are not obviously the right bit of metadata to keep
      ## All of these might want better names.
      discrete = dat$features$discrete,
      variable_order = NULL,
      output_order = NULL,
      ynames = NULL,
      n_out = NULL
    ),

    public = drop_null(list(
      ir = dat$ir,

      ## Methods:
      initialize = function(user = NULL, unused_user_action = NULL,
                            use_dde = FALSE) {
        ## TODO: why is 'use_dde' here in the initialiser and not in
        ## the run function?  We could take this as a default?
        ## Nothing looks like that would would be impossible.  Most
        ## likely we will need to support a period of the argument
        ## going here as well, with deprecation.
        private$use_dde <- use_dde || private$discrete
        private$data <- private$core$create()
        self$set_user(user = user, unused_user_action = unused_user_action)
        lockBinding("ir", self)
      },

      set_user = set_user,

      update = if (dat$features$discrete) {
        function(step, y) {
          if (private$delay) {
            stop("Can't call update() on delay models")
          }
          private$core$rhs_dde(step, y, private$data)
        }
      },

      deriv = if (!dat$features$discrete) {
        function(t, y) {
          if (private$delay && is.na(private$data$initial_t)) {
            private$data$initial_t <- t[[1]]
            on.exit(private$data$initial_t <- NA_real_)
          }
          ret <- private$core$rhs_dde(t, y, private$data)
          if (!is.null(private$core$output)) {
            attr(ret, "output") <- private$core$output(t, y, private$data)
          }
          ret
        }
      },

      initial = initial,

      run = run,

      contents = function() {
        sort_list(as.list(private$data))
      },

      transform_variables = function(y) {
        support_transform_variables(y, private)
      }
    )))

  generate_r_constructor(dat$config$base, dat$features$discrete, dat$user,
                         dat$ir, env)
}


generate_r_constructor <- function(base, discrete, user, ir, env) {
  name_user <- "user"
  if (length(user) > 0L) {
    i <- set_names(vlapply(user, "[[", "has_default"),
                   vcapply(user, "[[", "name"))
    nms <- names(i)
    args <- c(rep(alist(a = ), sum(!i)), rep(alist(a = NULL), sum(i)))
    names(args) <- nms
    args[[name_user]] <-
      as.call(c(list(quote(list)),
                set_names(lapply(nms, as.name), nms)))
    args["unused_user_action"] <- list(NULL)
    user_value <- as.name(name_user)
    unused_user_action <- quote(unused_user_action)
  } else {
    args <- alist()
    user_value <- NULL
    unused_user_action <- NULL
  }


  cl_init <- call("$", as.name(base), quote(new))
  call <- list(cl_init, user_value, unused_user_action)
  if (!discrete) {
    call <- c(call, list(quote(use_dde)))
    args <- c(args, alist(use_dde = FALSE))
  }

  ret <- as_function(args, r_expr_block(list(as.call(call))),
                     env %||% .GlobalEnv)
  if (!is.null(env)) {
    attr(ret, "ir") <- ir
    class(ret) <- "odin_generator"
  }
  ret
}
