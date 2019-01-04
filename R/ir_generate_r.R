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
  features_supported <- c("has_user", "has_output", "discrete")
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
    ## TODO: These 3 true/false pairs might be a ternary categorical arg?
    rhs_desolve = odin_ir_generate_rhs(eqs, dat, env, meta, TRUE, FALSE),
    rhs_dde = odin_ir_generate_rhs(eqs, dat, env, meta, FALSE, FALSE),
    output = odin_ir_generate_rhs(eqs, dat, env, meta, FALSE, TRUE))

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


odin_ir_generate_rhs <- function(eqs, dat, env, meta, desolve, output) {
  discrete <- dat$features$discrete
  has_output <- dat$features$has_output
  if (output && !has_output) {
    return(NULL)
  }
  if (discrete && (desolve || output)) {
    return(NULL)
  }

  include <- function(x) {
    ((output || desolve || discrete) && x$used$output) ||
      ((!output || desolve || discrete) && x$used$rhs)
  }

  f <- function(x) {
    if (include(x)) {
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
  alloc_result <- call("<-", meta$result,
                         call("numeric", call("length", meta$state)))
  alloc_output <- call("<-", meta$output,
                       call("numeric", dat$data$output$length))
  if (desolve || discrete) {
    if (has_output) {
      alloc <- list(alloc_result, alloc_output)
    } else {
      alloc <- list(alloc_result)
    }
  } else if (output) {
    alloc <- alloc_output
  } else {
    alloc <- alloc_result
  }

  eqs_include <- unname(eqs[vlapply(dat$equations, include)])

  if (desolve) {
    if (has_output) {
      ret <- call("list", meta[["result"]], meta[["output"]])
    } else {
      ret <- call("list", meta[["result"]])
    }
  } else if (output) {
    ret <- meta[["output"]]
  } else if (discrete && has_output) {
    ret <- list(
      call("<-", call("attr", meta[["result"]], "output"), meta[["output"]]),
      meta[["result"]])
  } else {
    ret <- meta[["result"]]
  }

  body <- as.call(c(list(quote(`{`)), c(vars, alloc, eqs_include, ret)))
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
    lhs <- call("[[", meta$result, pos)
  } else if (location == "output") {
    pos <- offset_to_position(dat$data$output$data[[eq$lhs$target]]$offset)
    lhs <- call("[[", meta$output, pos)
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
##
## TODO: The other way of doing this, which might be nicer, is for a
## top-level class and then inject dependencies into it.  We should be
## able to do this with minimal overhead and just as much of the class
## locked down to creation time.
odin_ir_generate_class <- function(core, dat, env, meta) {
  self <- private <- NULL # quieten global check: R6 adds these later
  if (dat$features$has_interpolate || dat$features$has_delay) {
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
      names = c(as.character(meta$time),
                dat$data$variable$order,
                dat$data$output$order),
      ## These are not obviously the right bit of metadata to keep
      variable_order = dat$data$variable$order,
      output_order = dat$data$output$order,
      discrete = dat$features$discrete,
      transform_variables = NULL
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

      update = if (dat$features$discrete) {
        function(step, y) {
          private$core$rhs_dde(step, y, private$data)
        }
      },

      deriv = if (!dat$features$discrete) {
        function(t, y) {
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

      run = if (dat$features$discrete) {
        function(step, y = NULL, ..., use_names = TRUE, replicate = NULL) {
          if (is.null(y)) {
            y <- self$initial(step)
          }
          if (is.null(replicate)) {
            ret <- dde::difeq(y, step, private$core$rhs_dde, private$data,
                              ynames = FALSE,
                              n_out = length(private$output_order), ...)
          } else {
            ret <- dde::difeq_replicate(
              replicate, y, step, private$core$rhs_dde, private$data,
              ynames = FALSE, output = private$core$output,
              n_out = length(private$output_order), ...)
          }
          if (use_names) {
            colnames(ret) <- private$names
          } else {
            colnames(ret) <- NULL
          }
          ret
        }
      } else {
        function(t, y = NULL, ..., use_names = TRUE) {
          if (is.null(y)) {
            y <- self$initial(t)
          }
          if (private$use_dde) {
            ret <- dde::dopri(y, t, private$core$rhs_dde, private$data,
                              ynames = FALSE, output = private$core$output,
                              n_out = length(private$output_order), ...)
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
        }
      },

      ## TODO: I am currently not sure if this belongs here or with
      ## the generator...
      ir = function() {
        private$ir_
      },

      contents = function() {
        sort_list(as.list(private$data))
      }
    )))

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


## Some support functions - these are not subject to code generation
## at all and will be injected into the appropriate environment.
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
