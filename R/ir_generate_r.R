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
                          "has_interpolate", "has_stochastic",
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
    get_user_double = as.name("_get_user_double"),
    get_user_dim = as.name("_get_user_dim"),
    check_interpolate_y = as.name("_check_interpolate_y"))

  ## This is our little rewriter - we'll tidy this up later
  rewrite <- function(x) {
    sexp_to_rexp(x, dat$data, meta)
  }

  eqs <- lapply(dat$equations, odin_ir_generate_expression, dat, meta, rewrite)
  names(eqs) <- vcapply(dat$equations, "[[", "name")

  ## Then start putting together the initial conditions
  env <- new.env(parent = odin_base_env())

  ## Support functions will come in this way:
  if (dat$features$has_user) {
    env[[as.character(meta$get_user_double)]] <- support_get_user_double
    env[[as.character(meta$get_user_dim)]] <- support_get_user_dim
  }
  if (dat$features$has_interpolate) {
    env[[as.character(meta$check_interpolate_y)]] <- support_check_interpolate_y
  }

  core <- list(
    create = odin_ir_generate_create(eqs, dat, env, meta),
    ic = odin_ir_generate_ic(eqs, dat, env, meta, rewrite),
    set_user = odin_ir_generate_set_user(eqs, dat, env, meta),
    ## TODO: These 3 true/false pairs might be a ternary categorical arg?
    rhs_desolve = odin_ir_generate_rhs(eqs, dat, env, meta, rewrite,
                                       TRUE, FALSE),
    rhs_dde = odin_ir_generate_rhs(eqs, dat, env, meta, rewrite,
                                   FALSE, FALSE),
    output = odin_ir_generate_rhs(eqs, dat, env, meta, rewrite,
                                  FALSE, TRUE),
    interpolate_t = odin_ir_generate_interpolate_t(
      dat, env, meta, rewrite),
    ## This one is a little different
    metadata = odin_ir_generate_metadata(dat, meta))

  odin_ir_generate_class(core, dat, env, meta)
}


odin_ir_generate_create <- function(eqs, dat, env, meta) {
  alloc <- call("<-", meta[["internal"]], quote(new.env(parent = emptyenv())))
  eqs_create <- flatten_eqs(eqs[dat$components$create$equations])
  ret <- meta[["internal"]]
  body <- as.call(c(list(quote(`{`)), c(alloc, eqs_create, ret)))
  as.function(c(alist(), body), env)
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
      seq <- call("seq_len", call("[[", meta$internal, d$dimnames$length))
      target <- call("[", meta$state, call("+", offset, seq))
    }
    call("<-", target, call("[[", meta$internal, x$initial))
  }
  assign <- lapply(dat$data$variable$contents, f)

  ## Build the function:
  body <- as.call(c(list(quote(`{`)), eqs_initial, alloc, assign, meta$state))
  args <- alist(time =, internal =)
  names(args)[[1]] <- as.character(meta$time)
  names(args)[[2]] <- as.character(meta$internal)
  as.function(c(args, body), env)
}


odin_ir_generate_set_user <- function(eqs, dat, env, meta) {
  eqs_user <- flatten_eqs(eqs[dat$components$user$equations])
  args <- alist(user =, internal =)
  names(args)[[1]] <- as.character(meta$user)
  names(args)[[2]] <- as.character(meta$internal)
  body <- as.call(c(list(quote(`{`)), eqs_user))
  as.function(c(args, body), env)
}


odin_ir_generate_rhs <- function(eqs, dat, env, meta, rewrite,
                                 desolve, output) {
  discrete <- dat$features$discrete
  has_output <- dat$features$has_output
  if (output && !has_output) {
    return(NULL)
  }
  if (discrete && (desolve || output)) {
    return(NULL)
  }

  ## TODO: there's an issue here where we combine entries from both
  ## rhs and output where the sort *might be broken.  But we'll find
  ## out if that's a problem later.
  use <- c(character(0),
           if (!output || desolve || discrete) "rhs",
           if ( output || desolve || discrete) "output")
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
      seq <- call("seq_len", call("[[", meta$internal, d$dimnames$length))
      extract <- call("[", meta$state, call("+", x$offset, seq))
      if (d$rank > 1L) {
        dims <- lapply(d$dimnames$dim, function(x) call("[[", meta$internal, x))
        extract <- call("array", extract, as.call(c(list(quote(c)), dims)))
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
  output_length <- rewrite(dat$data$output$length)
  alloc_output <- call("<-", meta$output, call("numeric", output_length))

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

  eqs_include <- flatten_eqs(eqs[use_eqs])

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


odin_ir_generate_metadata <- function(dat, meta) {
  ord1 <- function(x) {
    if (x$rank == 0L) {
      NULL
    } else if (x$rank == 1L) {
      call("[[", meta$internal, x$dimnames$length)
    } else {
      dims <- lapply(x$dimnames$dim, function(d) call("[[", meta$internal, d))
      as.call(c(list(quote(c)), dims))
    }
  }
  ord <- function(location) {
    len <- lapply(dat$data$data[names(dat$data[[location]]$contents)], ord1)
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
    body <- c(body,
              call("<-", quote(private$interpolate_t),
                   quote(private$core$interpolate_t(private$data))))
  }

  body <- as.call(c(list(as.name("{")), body))

  as.function(c(alist(self=, private =), list(body)), env)
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
  as.function(c(args, body), env)
}


## TODO: this should be generate_equation I think
##
## TODO: separate out the scalar and array types then move to a switch
## statement.
odin_ir_generate_expression <- function(eq, dat, meta, rewrite) {
  f <- switch(
    eq$type,
    alloc = odin_ir_generate_expression_alloc,
    alloc_interpolate = odin_ir_generate_expression_alloc_interpolate,
    copy = odin_ir_generate_expression_copy,
    user = odin_ir_generate_expression_user,
    expression_scalar = odin_ir_generate_expression_scalar,
    expression_array = odin_ir_generate_expression_array,
    stop("Unknown type"))

  data_info <- dat$data$data[[eq$lhs$target]]
  stopifnot(!is.null(data_info))

  f(eq, data_info, dat$data, meta, rewrite)
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
    } else if (fn == "norm_rand") {
      quote(rnorm(1L))
    } else {
      as.call(c(list(as.name(fn)), lapply(args, sexp_to_rexp, data, meta)))
    }
  } else if (is.character(x)) {
    if (x %in% data$internal) {
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


odin_ir_generate_expression_scalar <- function(eq, data_info, data, meta,
                                               rewrite) {
  location <- data_info$location

  if (location == "internal" || location == "transient") {
    lhs <- rewrite(eq$name)
  } else {
    offset <- data[[location]]$contents[[data_info$name]]$offset
    storage <- if (location == "variable") meta$result else meta$output
    lhs <- call("[[", storage, offset_to_position(offset))
  }

  rhs <- rewrite(eq$rhs$value)
  call("<-", lhs, rhs)
}


odin_ir_generate_expression_array <- function(eq, data_info, data, meta,
                                              rewrite) {
  lhs <- odin_ir_generate_expression_array_lhs(
    eq, data_info, data, meta, rewrite)
  lapply(eq$rhs, odin_ir_generate_expression_array_rhs,
         lhs, data_info, data, meta, rewrite)
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


odin_ir_generate_expression_alloc <- function(eq, data_info, data, meta,
                                              rewrite) {
  lhs <- call("[[", meta$internal, eq$lhs$target)
  alloc_fn <- switch(data_info$storage_type,
                     double = "numeric",
                     int = "integer",
                     stop(sprintf("unsupported storage type")))
  len <- call("[[", meta$internal, data_info$dimnames$length)
  rhs <- call(alloc_fn, len)
  if (data_info$rank > 1L) {
    dim <- as.call(c(quote(c), lapply(data_info$dimnames$dim, function(i)
      call("[[", meta$internal, i))))
    rhs <- call("array", rhs, dim)
  }
  call("<-", lhs, rhs)
}


odin_ir_generate_expression_alloc_interpolate <- function(eq, data_info,
                                                          data, meta,
                                                          rewrite) {
  name_target <- eq$lhs$target
  name_arg <- eq$interpolate$y

  data_info_target <- data$data[[name_target]]
  data_info_t <- data$data[[eq$interpolate$t]]
  data_info_arg <- data$data[[eq$interpolate$y]]

  len_t <- rewrite(data_info_t$dimnames$length)

  if (data_info$rank == 0L) {
    dim_arg <- rewrite(data_info_arg$dimnames$length)
    dim_target <- len_t
  } else {
    dim_arg <- call_c(lapply(data_info_arg$dimnames$dim, rewrite))
    if (data_info_target$rank == 1L) {
      dim_target <- call_c(c(len_t, rewrite(data_info_target$dimnames$length)))
    } else {
      dim_target <-
        call_c(c(len_t, lapply(data_info_target$dimnames$dim, rewrite)))
    }
  }

  check <- call(as.character(meta$check_interpolate_y),
                dim_arg, dim_target, name_arg, name_target)

  lhs <- call("[[", meta$internal, eq$name)
  args <- list(quote(cinterpolate::interpolation_function),
               rewrite(eq$interpolate$t),
               rewrite(eq$interpolate$y),
               eq$interpolate$type,
               scalar = TRUE)
  rhs <- as.call(args)
  list(check, call("<-", lhs, rhs))
}


odin_ir_generate_expression_copy <- function(eq, data_info, data, meta,
                                             rewrite) {
  ## NOTE: this applies only to coping a variable into the output
  offset <- rewrite(data$output$contents[[eq$lhs$target]]$offset)
  storage <- meta$output

  if (data_info$rank == 0) {
    lhs <- call("[[", storage, offset_to_position(offset))
  } else{
    i <- call("seq_len",
              call("[[", meta$internal, data_info$dimnames$length))
    lhs <- call("[", storage, call("+", offset, i))
  }

  rhs <- rewrite(eq$lhs$target)
  call("<-", lhs, rhs)
}


## NOTE: There are two entirely separate codepaths here so this could
## be factored out again (and probably should be).
odin_ir_generate_expression_user <- function(eq, data_info, data, meta,
                                             rewrite) {
  if (eq$user$dim) {
    name <- eq$name
    len <- data_info$dimnames$length
    if (data_info$rank == 1L) {
      dims <- NULL
    } else {
      dims <- as.call(c(list(quote(c)), data_info$dimnames$dim))
    }
    call(as.character(meta$get_user_dim), meta$user, meta$internal,
         name, len, dims)
  } else {
    name <- eq$name
    lhs <- call("[[", meta$internal, name)
    rank <- data_info$rank
    if (is.null(eq$user$default)) {
      default <- NULL
    } else {
      default <- rewrite(eq$user$default)
    }
    if (rank == 0L) {
      size <- NULL
    } else if (rank == 1L) {
      size <- call("[[", meta$internal, data_info$dimnames$length)
    } else {
      dim <- lapply(data_info$dimnames$dim, function(x)
        call("[[", meta$internal, x))
      size <- as.call(c(list(quote(c)), dim))
    }
    rhs <- call(as.character(meta$get_user_double),
                meta$user, name, meta$internal, size, default)
    call("<-", lhs, rhs)
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
  if (dat$features$has_delay) {
    stop("more tweaks needed here...")
  }
  if (dat$features$has_interpolate) {
    loadNamespace("cinterpolate")
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
            y <- self$initial(step[[1L]])
          }
          if (private$interpolate) {
            support_check_interpolate_t(step, private$interpolate_t, NULL)
          }
          if (is.null(replicate)) {
            ret <- dde::difeq(y, step, private$core$rhs_dde, private$data,
                              ynames = FALSE, n_out = private$n_out, ...)
          } else {
            ret <- dde::difeq_replicate(
              replicate, y, step, private$core$rhs_dde, private$data,
              ynames = FALSE, output = private$core$output,
              n_out = private$n_out, ...)
          }
          if (use_names) {
            colnames(ret) <- private$ynames
          } else {
            colnames(ret) <- NULL
          }
          ret
        }
      } else {
        function(t, y = NULL, ..., use_names = TRUE, tcrit = NULL) {
          if (is.null(y)) {
            y <- self$initial(t[[1L]])
          }
          if (private$interpolate) {
            tcrit <-
              support_check_interpolate_t(t, private$interpolate_t, tcrit)
          }
          if (private$use_dde) {
            ## TODO: there's a second type of critical time that
            ## should be enabled here (but is not yet in mainstream
            ## odin) -- all switch points in constant interpolations.
            ## Let's not do this yet though.
            ret <- dde::dopri(y, t, private$core$rhs_dde, private$data,
                              ynames = FALSE, output = private$core$output,
                              n_out = private$n_out, ...)
          } else {
            ret <- deSolve::ode(y, t, private$core$rhs_desolve, private$data,
                                tcrit = tcrit, ...)
          }
          if (use_names) {
            colnames(ret) <- private$ynames
          } else {
            dimnames(ret) <- NULL
            class(ret) <- "matrix"
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

  as.function(c(args, body), env)
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


call_c <- function(x) {
  as.call(c(list(quote(c)), x))
}
