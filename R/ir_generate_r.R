## These two are temporary!
odin2 <- function(x, validate = FALSE) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin2_(xx, validate)
}


odin2_ <- function(x, validate = FALSE) {
  ir <- odin_build_ir(x)
  odin_ir_generate(ir, validate)
}


## TODO: this needs a bunch of naming work - currently the prefix here
## is "odin_ir_generate" but this is "ir -> r" - we'll have "ir -> c"
## and eventually "ir -> js" here so we'll probably move to something
## like "gen_<target>_xxx" once this is working properly

odin_ir_generate <- function(ir, validate = TRUE) {
  if (validate) {
    ir_validate(ir)
  }
  dat <- ir_deserialise(ir)
  dat$ir <- ir

  ## Pull this out into something generally useful
  features_supported <- c("has_user")
  features_used <- vlapply(dat$features, identity)
  msg <- setdiff(names_if(features_used), features_supported)
  if (length(msg) > 0L) {
    stop("Features not suppored: ", paste(dquote(msg), collapse = ", "))
  }

  ## TODO: this is just punting for later
  ##
  ## NOTE: 'state' here is where *variables* are put.  This probably
  ## wants tightening up later...
  meta <- list(internal = quote(INTERNAL),
               user = as.name(USER),
               state = as.name(STATE),
               dstatedt = as.name(DSTATEDT),
               time = as.name(TIME),
               get_user_double = as.name("_get_user_double"))

  eqs <- lapply(dat$equations, odin_ir_generate_expression, dat, meta)
  names(eqs) <- vcapply(dat$equations, "[[", "name")

  ## Then start putting together the initial conditions

  ## It's not clear what the correct environment here should be, so
  ## let's start with the simplest environment first.
  env <- new.env(parent = as.environment("package:base"))

  ## Support functions will come in this way:
  if (dat$features$has_user) {
    env[[as.character(meta$get_user_double)]] <- support_get_user_double
  }

  core <- list(
    create = odin_ir_generate_create(eqs, dat, env, meta),
    ic = odin_ir_generate_ic(eqs, dat, env, meta),
    set_user = odin_ir_generate_set_user(eqs, dat, env, meta),
    rhs_desolve = odin_ir_generate_rhs(eqs, dat, env, meta, TRUE),
    rhs_dde = odin_ir_generate_rhs(eqs, dat, env, meta, FALSE))

  odin_ir_generate_class(core, dat, env, meta)
}


odin_ir_generate_create <- function(eqs, dat, env, meta) {
  user <- lapply(dat$data$user, function(x)
    call("<-", call("[[", meta[["internal"]], x$name), x$default_value))
  alloc <- call("<-", meta[["internal"]], quote(new.env(parent = emptyenv())))
  eqs_create <- unname(eqs[vlapply(dat$equations, function(eq) eq$used$create)])
  ret <- meta[["internal"]]
  body <- as.call(c(list(quote(`{`)), c(alloc, user, eqs_create, ret)))
  as.function(c(alist(), body), env)
}


## TODO: 'ic' ==> 'initial'
odin_ir_generate_ic <- function(eqs, dat, env, meta) {
  if (dat$features$has_array) {
    stop("ic will need work (features$has_array)")
  }
  if (dat$data$variable$length_is_var) {
    stop("ic will need work (variable$length_is_var)")
  }
  if (dat$data$variable$length_stage > STAGE_CONSTANT) {
    stop("ic will need work (variable$length_stage)")
  }

  alloc <- call("<-", meta$state,
                call("numeric", dat$data$variable$length))
  ## These are only time dependent things
  eqs_initial <-
    unname(eqs[vlapply(dat$equations, function(eq) eq$used$initial)])

  f <- function(x) {
    call("<-",
         call("[[", meta$state, offset_to_position(x$offset)),
         call("[[", meta$internal, initial_name(x$name)))
  }
  assign <- lapply(dat$data$variable$data[dat$data$variable$order], f)
  ret <- meta$state

  body <- as.call(c(list(quote(`{`)), eqs_initial, alloc, assign, ret))
  args <- alist(time =, internal =)
  names(args)[[1]] <- as.character(meta$time)
  names(args)[[2]] <- as.character(meta$internal)
  as.function(c(args, body), env)
}


odin_ir_generate_set_user <- function(eqs, dat, env, meta) {
  user <- unname(lapply(dat$data$user, function(x)
    call(as.character(meta$get_user_double),
         meta$user, x$name, meta$internal)))
  eqs_user <-
    unname(eqs[vlapply(dat$equations, function(eq) eq$used$user)])

  args <- alist(user =, internal =)
  names(args)[[1]] <- as.character(meta$user)
  names(args)[[2]] <- as.character(meta$internal)
  body <- as.call(c(list(quote(`{`)), user, eqs_user))
  as.function(c(args, body), env)
}


odin_ir_generate_rhs <- function(eqs, dat, env, meta, desolve) {
  f <- function(x) {
    if (x$used$rhs) {
      call("<-", as.name(x$name),
           call("[[", meta$state, offset_to_position(x$offset)))
    }
  }
  vars <- unname(drop_null(lapply(dat$data$variable$data, f)))

  ## NOTE: There are two reasonable things to do here - we can look up
  ## the length of the variable (dat$data$variable$length) or we can
  ## just make this a vector the same length as the incoming state (as
  ## dydt is always the same length as y).  Neither seems much better
  ## than the other, so going with the same length approach here as it
  ## is less logic and will work for variable-length cases.
  alloc <- call("<-", meta$dstatedt,
                call("numeric", call("length", meta$state)))

  eqs_rhs <- unname(eqs[vlapply(dat$equations, function(eq) eq$used$rhs)])

  if (desolve) {
    ret <- call("list", meta[["dstatedt"]])
  } else {
    ret <- meta[["dstatedt"]]
  }

  body <- as.call(c(list(quote(`{`)), c(vars, alloc, eqs_rhs, ret)))
  args <- alist(t = , y =, parms = )
  names(args)[[1]] <- as.character(meta$time)
  names(args)[[2]] <- as.character(meta$state)
  names(args)[[3]] <- as.character(meta$internal)
  as.function(c(args, body), env)
}


odin_ir_generate_expression <- function(eq, dat, meta) {
  st <- eq$stage
  nm <- eq$name

  location <- eq$lhs$location

  ## LHS:
  if (location == "internal") {
    if (dat$data$internal$data[[nm]]$transient) {
      lhs <- as.name(nm)
    } else {
      lhs <- call("[[", meta$internal, nm)
    }
  } else if (location == "variable") {
    pos <- offset_to_position(dat$data$variable$data[[eq$lhs$target]]$offset)
    lhs <- call("[[", meta$dstatedt, pos)
  } else {
    stop("Unhandled path")
  }

  ## RHS:
  if (eq$rhs$type == "expression") {
    ## TODO: this should be put into internal I think?
    internal <- names_if(!vlapply(dat$data$internal$data, "[[", "transient"))
    rhs <- sexp_to_rexp(eq$rhs$value, internal, meta)
  } else if (eq$rhs$type == "atomic") {
    rhs <- eq$rhs$value
  }

  call("<-", lhs, rhs)
}


sexp_to_rexp <- function(x, internal, meta) {
  if (is.recursive(x)) {
    as.call(c(list(as.name(x[[1L]])),
              lapply(x[-1L], sexp_to_rexp, internal, meta)))
  } else if (is.character(x)) {
    if (x %in% internal) {
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
odin_ir_generate_class <- function(core, dat, env, meta) {
  if (dat$features$has_output ||
      dat$features$has_interpolate || dat$features$has_delay ||
      dat$features$discrete) {
    stop("more tweaks needed here...")
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
      ## TODO: this might change:
      initial_time_dependent = dat$data$initial$stage == "time",
      ## TODO: this is a horrible name
      ir_ = dat$ir,
      variable_order = dat$data$variable$order,
      names = c(as.character(meta$time), dat$data$variable$order),
      transform_variables = NULL),

    public = list(
      ## Methods:
      initialize = function(user = NULL, use_dde = FALSE) {
        if (use_dde) {
          loadNamespace(dde)
        }
        private$use_dde <- use_dde

        private$data <- private$core$create()
        private$core$set_user(user, private$data)

        if (!private$initial_time_dependent) {
          private$init <- private$core$ic(NA_real_, private$data)
        }

        ## TODO: odin_prepare here as that sorts out even more stuff -
        ## this is currently done within update_cache in the existing
        ## version.
      },

      set_user = function(..., user = list(...)) {
        private$core$set_user(user)
        if (!private$initial_time_dependent) {
          private$init <- private$core$ic(NA_real_, private$data)
        }
      },

      deriv = function(t, y) {
        ## TODO: not sure in the face of output variables what to do
        ## here, or what we do already...
        private$core$rhs_dde(t, y, private$data)
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

      run = function(t, y = NULL, ..., use_names = TRUE) {
        if (is.null(y)) {
          y <- self$initial(t)
        }
        if (private$use_dde) {
          ret <- dde::dopri(y, t, private$core$rhs_dde, private$data,
                            ynames = FALSE, ...)
        } else {
          ret <- deSolve::ode(y, t, private$core$rhs_desolve, private$data,
                              ...)
        }
        if (use_names) {
          colnames(ret) <- private$names
        } else {
          colnames(ret) <- NULL
        }
        ret
      },

      ## TODO: I am currently not sure if this belongs here or with
      ## the generator...
      ir = function() {
        private$ir_
      },

      contents = function() {
        res <- as.list(private$data)
        res[order(names(res))]
      }
    ))

  cl_init <- call("$", as.name(dat$config$base), quote(new))
  ## Need to build a nice argument list here.  This is pretty ugly and
  ## will be somewhat duplicated with different interfaces.
  ##
  ## TODO: this is not going to reliably preserve ordering of arguments.
  if (dat$features$has_user) {
    i <- vlapply(dat$data$user, function(x) x$has_default)
    nms <- names(i)[order(i)]
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

  as.function(c(args, body), env)
}


## Some support functions:
support_get_user_double <- function(user, name, internal) {
  given <- user[[name]]
  if (is.null(given)) {
    if (is.null(internal[[name]])) {
      stop(sprintf("Expected a value for '%s'", name), call. = FALSE)
    }
  } else {
    if (length(given) != 1L) {
      stop(sprintf("Expected a scalar numeric for '%s'", name), call. = FALSE)
    }
    if (is.integer(given)) {
      given <- as.numeric(given)
    } else if (!is.numeric(given)) {
      stop(sprintf("Expected a numeric value for '%s'", name), call. = FALSE)
    }
    internal[[name]] <- given
  }
}
