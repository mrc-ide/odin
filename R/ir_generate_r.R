## TODO: this needs a bunch of naming work - currently the prefix here
## is "odin_ir_generate" but this is "ir -> r" - we'll have "ir -> c"
## and eventually "ir -> js" here so we'll probably move to something
## like "gen_<target>_xxx" once this is working properly

odin_ir_generate <- function(ir, safe, validate = TRUE) {
  if (validate) {
    ir_validate(ir)
  }
  dat <- from_json(ir)

  features_supported <- character()
  features_used <- vlapply(dat$features, identity)
  msg <- setdiff(names(features_used)[features_used], features_supported)
  if (length(msg) < 0L) {
    stop("Features not suppored: ", paste(dquote(msg), collapse = ", "))
  }

  ## TODO: this is just punting for later
  ##
  ## NOTE: 'state' here is where *variables* are put.  This probably
  ## wants tightening up later...
  meta <- list(internal = quote(INTERNAL),
               parameters = quote(PARAMETERS),
               state = quote(STATE),
               dstate = quote(DSTATE),
               time = as.name(TIME))

  eqs <- lapply(dat$equations, odin_ir_generate_expression, dat, meta)
  names(eqs) <- vcapply(dat$equations, "[[", "name")

  ## Then start putting together the initial conditions

  ## It's not clear what the correct environment here should be, so
  ## let's start with the simplest environment first.
  env <- new.env(parent = as.environment("package:base"))
  core <- list(
    create = odin_ir_generate_create(eqs, dat, env, meta),
    ic = odin_ir_generate_ic(eqs, dat, env, meta),
    set_user = odin_ir_generate_set_user(eqs, dat, env, meta),
    rhs_desolve = odin_ir_generate_rhs(eqs, dat, env, meta, TRUE),
    rhs_dde = odin_ir_generate_rhs(eqs, dat, env, meta, FALSE))
  odin_ir_generate_class(core, dat, env, meta)
}


odin_ir_generate_create <- function(eqs, dat, env, meta) {
  ## NOTE: this is not the most efficient way of doing this; we should
  ## preallocate by looking up the names that the assignments will go
  ## to.  This is not a major cost, so for now it's not worth the
  ## complexity while we sort things out.  If the internal structure
  ## becomes an environment it won't be needed.
  eqs_create <- unname(eqs[vlapply(dat$equations, function(eq) eq$used$create)])
  body <- as.call(c(list(quote(`{`)),
                    c(call("<-", meta[["internal"]], quote(list())),
                      eqs_create,
                      meta[["internal"]])))
  as.function(c(alist(), body), env)
}


odin_ir_generate_ic <- function(eqs, dat, env, meta) {
  if (dat$features$has_array) {
    stop("ic will need work (has_array)")
  }
  if (dat$data$variable$length_is_var) {
    stop("ic will need work (length_is_var)")
  }
  if (dat$data$variable$length_stage > STAGE_CONSTANT) {
    stop("ic will need work (length_stage)")
  }

  vars <- dat$data$variable$data[dat$data$variable$order]

  f <- function(x) {
    call("<-",
         call("[[", meta$state, offset_to_position(x$offset)),
         call("[[", meta$internal, initial_name(x$name)))
  }

  alloc <- call("<-", meta$state,
                call("numeric", dat$data$variable$length))
  body <- as.call(c(list(quote(`{`)),
                    alloc, lapply(vars, f), as.name(meta$state)))
  args <- alist(time =, internal =)
  names(args)[[1]] <- as.character(meta$time)
  names(args)[[2]] <- as.character(meta$internal)
  as.function(c(args, body), env)
}


odin_ir_generate_set_user <- function(eqs, dat, env, meta) {
  if (dat$features$has_user) {
    stop("This function needs updating")
  }
  args <- alist(t =, params =)
  names(args)[[1]] <- as.character(meta$time)
  names(args)[[2]] <- as.character(meta$parameters)
  body <- call("{")
  as.function(c(args, body), env)
}


odin_ir_generate_rhs <- function(eqs, dat, env, meta, desolve) {
  vars <- lapply(dat$data$variable$data, function(x)
    if (x$used$rhs) call("<-", as.name(x$name), call("[[", meta$state, x$name)))
  vars <- unname(drop_null(vars))

  ## NOTE: There are two reasonable things to do here - we can look up
  ## the length of the variable (dat$data$variable$length) or we can
  ## just make this a vector the same length as the incoming state (as
  ## dydt is always the same length as y).  Neither seems much better
  ## than the other, so going with the same length approach here as it
  ## is less logic and will work for variable-length cases.
  alloc <- call("<-", meta$dstate,
                call("numeric", call("length", meta$state)))

  eqs_rhs <- unname(eqs[vlapply(dat$equations, function(eq) eq$used$rhs)])

  if (desolve) {
    ret <- call("list", meta[["dstate"]])
  } else {
    ret <- meta[["dstate"]]
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
    lhs <- call("[[", meta$internal, nm)
  } else if (location == "variable") {
    pos <- offset_to_position(dat$data$variable$data[[eq$lhs$target]]$offset)
    lhs <- call("[[", meta$dstate, pos)
  } else {
    stop("Unhandled path")
  }

  ## RHS:
  if (eq$rhs$type == "expression") {
    rhs <- sexp_to_rexp(eq$rhs$value, names(dat$data$internal$data), meta)
  } else if (eq$rhs$type == "atomic") {
    rhs <- eq$rhs$value
  }

  call("<-", lhs, rhs)
}


sexp_to_rexp <- function(x, internal, meta) {
  if (is.recursive(x)) {
    browser()
  } else if (is.character(x)) {
    if (x %in% internal) {
      call("[[", meta$internal, x)
    } else {
      as.name(x)
    }
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
  if (dat$features$has_user || dat$features$has_output ||
      dat$features$has_interpolate || dat$features$has_delay ||
      dat$features$discrete) {
    stop("more tweaks needed here...")
  }
  ## TODO: can't detect if initial stage is important here.

  env[[dat$config$base]] <- R6::R6Class(
    ## TODO: use of 'odin_model' here is somewhat incorrect because
    ## the objects are not really substituable within a class.  This
    ## should probably come from the base name
    "odin_model",
    parent_env = environment(odin),
    cloneable = FALSE,
    public = list(
      name = dat$config$base,
      core = core,
      internal = NULL,
      use_dde = NULL,
      ## Cache:
      init = NULL,
      variable_order = dat$data$variable$order,
      names = c(as.character(meta$time), dat$data$variable$order),
      transform_variables = NULL,
      ## Methods:
      initialize = function(use_dde = FALSE) {
        if (use_dde) {
          loadNamespace(dde)
        }
        self$use_dde <- use_dde

        self$internal <- self$core$create()
        ## TODO: only works if initial stage is 'constant'
        self$init <- self$core$ic(NA_real_, self$internal)

        ## TODO: odin_prepare here as that sorts out even more stuff -
        ## this is currently done within update_cache in the existing
        ## version.

        ## Seal class (TODO: consider moving these to be private?)
        lockBinding(quote(core), self)
        lockBinding(quote(variable_order), self)
        lockBinding(quote(names), self)
        lockBinding(quote(use_dde), self)
        lockBinding(quote(init), self)
      },

      deriv = function(t, y) {
        ## TODO: not sure in the face of output variables what to do
        ## here, or what we do already...
        self$core$rhs_dde(t, y, self$internal)
      },

      initial = function(t) {
        self$init
      },

      run = function(t, y = NULL, ..., use_names = TRUE) {
        if (is.null(y)) {
          y <- self$init
        }
        if (self$use_dde) {
          ret <- dde::dopri(y, t, self$core$rhs_dde, self$internal,
                            ynames = FALSE, ...)
        } else {
          ret <- deSolve::ode(y, t, self$core$rhs_desolve, self$internal, ...)
        }
        if (use_names) {
          colnames(ret) <- self$names
        } else {
          colnames(ret) <- NULL
        }
        ret
      }
    ))

  cl_init <- call("$", as.name(dat$config$base), quote(new))
  body <- call("{", as.call(list(cl_init, quote(use_dde))))
  args <- alist(use_dde = FALSE)
  as.function(c(args, body), env)
}
