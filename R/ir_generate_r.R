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
  features_supported <- c("has_user", "has_output", "discrete", "has_array")
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
    index = lapply(INDEX, as.name),
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
    output = odin_ir_generate_rhs(eqs, dat, env, meta, FALSE, TRUE),
    ## This one is a little different
    metadata = odin_ir_generate_metadata(eqs, dat, env, meta))

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
  if (dat$data$variable$length_stage > STAGE_CONSTANT) {
    stop("ic will need work (variable$length_stage)")
  }

  var_length <-
    sexp_to_rexp(dat$data$variable$length, dat$data$internal$contents, meta)
  alloc <- call("<-", meta$state, call("numeric", var_length))
  ## These are only time dependent things
  eqs_initial <-
    unname(eqs[vlapply(dat$equations, function(eq) eq$used$initial)])

  f <- function(x) {
    if (x$rank == 0) {
      target <- call("[[", meta$state, offset_to_position(x$offset))
    } else {
      seq <- call("seq_len", call("[[", meta$internal, array_dim_name(x$name)))
      target <- call("[", meta$state, call("+", x$offset, seq))
    }
    call("<-", target, call("[[", meta$internal, initial_name(x$name)))
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
      if (x$rank == 0L) {
        extract <- call("[[", meta$state, offset_to_position(x$offset))
      } else {
        seq <- call("seq_len",
                    call("[[", meta$internal, array_dim_name(x$name)))
        extract <- call("[", meta$state, call("+", x$offset, seq))
        if (x$rank > 1L) {
          dim <- as.call(c(
            list(quote(c)),
            lapply(seq_len(x$rank), function(i)
              call("[[", meta$internal, array_dim_name(x$name, i)))))
          extract <- call("array", extract, dim)
        }
      }
      call("<-", as.name(x$name), extract)
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


odin_ir_generate_metadata <- function(eqs, dat, env, meta) {
  f <- function(x) {
    if (x$rank == 0L) {
      NULL
    } else if (x$rank == 1L) {
      call("[[", meta$internal, array_dim_name(x$name))
    } else {
      as.call(c(list(quote(c)),
                lapply(seq_len(x$rank), function(i)
                  call("[[", meta$internal, array_dim_name(x$name, i)))))
    }
  }
  ord <- function(x) {
    as.call(c(list(quote(list)), lapply(x$data[x$order], f)))
  }
  ynames <- call(
    "make_names2",
    quote(private$variable_order), quote(private$output_order),
    dat$features$discrete)
  n_out <- call("support_n_out", quote(support_n_out(private$output_order)))

  ## Don't use the given environment but a different one (may change
  ## in future).
  env2 <- new.env(parent = environment(odin))
  body <- call(
    "{",
    call("<-", meta$internal, quote(private$data)),
    call("<-", quote(private$variable_order), ord(dat$data$variable)),
    call("<-", quote(private$output_order), ord(dat$data$output)),
    call("<-", call("$", quote(private), quote(ynames)), ynames),
    call("<-", call("$", quote(private), quote(n_out)), n_out))
  as.function(c(alist(self=, private =), list(body)), env2)
}


## TODO: this one looks like it breaks apart into different features
## as there are two big if/else blocks here.  There are a few ways of
## breaking this up though, and it looks like array/nonarray is better
## than lhs/rhs
odin_ir_generate_expression <- function(eq, dat, meta) {
  st <- eq$stage
  nm <- eq$name

  location <- eq$lhs$location
  internal <- dat$data$internal$contents

  ## TODO: this might move into the ir:
  data_name <- if (location == "internal") nm else eq$lhs$target
  data_info <- dat$data[[location]]$data[[data_name]]

  ## LHS:
  if (location == "internal") {
    ## TODO: I think that this is equivalent to
    ##   lhs <- sexp_to_rexp(eq$rhs$value, internal, meta)
    if (dat$data$internal$data[[nm]]$transient) {
      lhs <- as.name(nm)
    } else if (eq$type == "array_expression") {
      storage <- call("[[", meta$internal, nm)
      if (data_info$rank == 1L) {
        lhs <- call("[[", storage, meta$index[[1]])
      } else {
        lhs <- as.call(c(list(quote(`[`), storage),
                         meta$index[seq_len(data_info$rank)]))
      }
    } else {
      lhs <- call("[[", meta$internal, nm)
    }
  } else if (eq$type == "array_expression") {
    ## TODO: 'result' becomes 'dstatedt' (a little complicated by
    ## location above - consider replacing dstatedt with result!)
    offset <- sexp_to_rexp(data_info$offset, internal, meta)
    storage <- if (location == "variable") meta$result else meta$output
    ## TODO: in the C version this is all done in rewrite and that
    ## might be a better place to put it frankly.
    if (data_info$rank == 1L) {
      index <- meta$index[[1L]]
    } else {
      ## TODO: once things are sorted out this is prime for tidying
      ## up!  This is doing a lot of the bits that rewrite could just
      ## as easily do, really.
      f <- function(i) {
        if (i == 1) {
          meta$index[[i]]
        } else {
          n <- array_dim_name(data_info$name,
                              paste(seq_len(i - 1), collapse = ""))
          call("*", call("[[", meta$internal, n),
               call("-", meta$index[[i]], 1L))
        }
      }
      index <- collapse_expr(lapply(seq_len(data_info$rank), f), "+")
    }
    lhs <- call("[[", storage,
                if (identical(offset, 0)) index else call("+", index, offset))
  } else if (location == "variable") {
    lhs <- call("[[", meta$result, offset_to_position(data_info$offset))
  } else if (location == "output") {
    lhs <- call("[[", meta$output, offset_to_position(data_info$offset))
  } else {
    stop("Unhandled path")
  }

  if (eq$type == "scalar_expression") {
    rhs <- sexp_to_rexp(eq$rhs$value, internal, meta)
    call("<-", lhs, rhs)
  } else if (eq$type == "array_expression") {
    ## TODO: we can do better here on translation when we have ':' but
    ## this can go into sexp_to_rexp - use seq_along and seq_len where
    ## appropriate, but the gains from that will be small compared
    ## with avoiding vectorisation.
    ##
    ## TODO: this is just not going to work for multi-dimensional
    ## arrays!
    ##
    ## TODO: we can (re-)vectorise lots of expressions here.
    f <- function(i) {
      expr_body <- call("<-", lhs,
                        sexp_to_rexp(eq$rhs$value[[i]], internal, meta))
      subs <- list()
      for (j in rev(seq_along(eq$lhs$index[[i]]$value))) {
        if (eq$lhs$index[[i]]$is_range[[j]]) {
          expr_index <-
            sexp_to_rexp(eq$lhs$index[[i]]$value[[j]], internal, meta)
          expr_body <- call("for", meta$index[[j]], expr_index,
                            call("{", expr_body))
        } else if (length(eq$lhs$index[[i]]$value[[j]]) == 1L) {
          subs[[as.character(meta$index[[j]])]] <-
            sexp_to_rexp(eq$lhs$index[[i]]$value[[j]], internal, meta)
        } else {
          stop("Nontrivial non-range array access")
        }
      }
      if (length(subs) > 0L) {
        expr_body <- substitute_(expr_body, subs)
      }
      expr_body
    }

    if (location == "internal") {
      ## TODO: look at data_info$storage_type for more storage options
      alloc_rhs <- call("numeric",
                        call("[[", meta$internal, array_dim_name(nm)))
      if (data_info$rank > 1L) {
        dim <- as.call(c(list(quote(c)),
                         lapply(seq_len(data_info$rank), function(i)
                           call("[[", meta$internal, array_dim_name(nm, i)))))
        alloc_rhs <- call("array", alloc_rhs, dim)
      }
      alloc <- list(call("<-", call("[[", meta$internal, eq$name), alloc_rhs))
    } else {
      alloc <- NULL
    }

    res <- c(alloc, lapply(seq_along(eq$lhs$index), f))
    if (length(res) == 1L) {
      res[[1L]]
    } else {
      as.call(c(list(quote(`{`)), res))
    }
  } else if (eq$type == "dim") {
    ## TODO: in the IR we really need to indicate where the *target*
    ## lives - we need to know the target rank here but can't easily
    ## get at it!  For now I will punt on that and infer it, but this
    ## information belongs in the IR.
    rank <- length(eq$rhs$value)
    if (rank == 1L) {
      rhs <- sexp_to_rexp(eq$rhs$value[[1L]], internal, meta)
      call("<-", lhs, rhs)
    } else {
      ## TODO: this makes a _total_ mess of the lhs; this function
      ## needs major work!
      nm_target <- eq$lhs$target
      dimnames <- vcapply(seq_len(rank), array_dim_name, name = nm_target)
      dimdata <- lapply(dimnames, function(x) call("[[", meta$internal, x))
      dim1 <- lapply(seq_len(rank), function(i)
        call("<-", dimdata[[i]],
             sexp_to_rexp(eq$rhs$value[[i]], internal, meta)))
      dim <- call("<-", lhs, collapse_expr(dimdata, "*"))
      if (rank >= 3) {
        dim2 <- lapply(3:rank, function(i) {
          k <- seq_len(i - 1L)
          call("<-",
               call("[[", meta$internal,
                    array_dim_name(nm_target, paste(k, collapse = ""))),
               collapse_expr(dimdata[k], "*"))
        })
      } else {
        dim2 <- NULL
      }
      as.call(c(list(quote(`{`)), c(dim1, dim2, dim)))
    }
  } else {
    stop("Unhandled type")
  }
}


sexp_to_rexp <- function(x, internal, meta) {
  if (is.recursive(x)) {
    fn <- x[[1L]]
    args <- x[-1L]
    if (fn == "length") {
      sexp_to_rexp(array_dim_name(args[[1L]]), internal, meta)
    } else if (fn == "dim") {
      sexp_to_rexp(array_dim_name(args[[1L]], args[[2L]]), internal, meta)
    } else {
      as.call(c(list(as.name(fn)),
                lapply(args, sexp_to_rexp, internal, meta)))
    }
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
            y <- self$initial(step)
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
        function(t, y = NULL, ..., use_names = TRUE) {
          if (is.null(y)) {
            y <- self$initial(t)
          }
          if (private$use_dde) {
            ret <- dde::dopri(y, t, private$core$rhs_dde, private$data,
                              ynames = FALSE, output = private$core$output,
                              n_out = private$n_out, ...)
          } else {
            ret <- deSolve::ode(y, t, private$core$rhs_desolve, private$data,
                                ...)
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


collapse_expr <- function(expr, join) {
  ret <- expr[[1L]]
  for (i in seq_along(expr)[-1L]) {
    ret <- call(join, ret, expr[[i]])
  }
  ret
}
