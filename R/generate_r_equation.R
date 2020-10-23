generate_r_equations <- function(dat, rewrite) {
  lapply(dat$equations, generate_r_equation, dat, rewrite)
}


generate_r_equation <- function(eq, dat, rewrite) {
  f <- switch(
    eq$type,
    expression_scalar = generate_r_equation_scalar,
    expression_inplace = generate_r_equation_inplace,
    expression_array = generate_r_equation_array,
    alloc = generate_r_equation_alloc,
    alloc_interpolate = generate_r_equation_alloc_interpolate,
    alloc_ring = generate_r_equation_alloc_ring,
    copy = generate_r_equation_copy,
    interpolate = generate_r_equation_interpolate,
    user = generate_r_equation_user,
    delay_index = generate_r_equation_delay_index,
    delay_continuous = generate_r_equation_delay_continuous,
    delay_discrete = generate_r_equation_delay_discrete,
    stop("Unknown type"))

  data_info <- dat$data$elements[[eq$lhs]]
  stopifnot(!is.null(data_info))

  f(eq, data_info, dat, rewrite)
}


generate_r_equation_scalar <- function(eq, data_info, dat, rewrite) {
  location <- data_info$location

  if (location == "internal" || location == "transient") {
    lhs <- rewrite(eq$lhs)
  } else {
    offset <- dat$data[[location]]$contents[[data_info$name]]$offset
    storage <- if (location == "variable") dat$meta$result else dat$meta$output
    lhs <- call("[[", as.name(storage), r_offset_to_position(offset))
  }

  rhs <- rewrite(eq$rhs$value)
  call("<-", lhs, rhs)
}


generate_r_equation_inplace <- function(eq, data_info, dat, rewrite) {
  location <- data_info$location
  lhs <- rewrite(eq$lhs)
  rhs <- rewrite(eq$rhs$value)
  call("<-", lhs, rhs)
}


generate_r_equation_array <- function(eq, data_info, dat, rewrite) {
  lhs <- generate_r_equation_array_lhs(eq, data_info, dat, rewrite)
  lapply(eq$rhs, function(x)
    generate_r_equation_array_rhs(x$value, x$index, lhs, rewrite))
}


generate_r_equation_alloc <- function(eq, data_info, dat, rewrite) {
  lhs <- rewrite(eq$lhs)
  alloc_fn <- switch(data_info$storage_type,
                     double = "numeric",
                     int = "integer",
                     stop(sprintf("unsupported storage type")))
  len <- rewrite(data_info$dimnames$length)
  rhs <- call(alloc_fn, len)
  if (data_info$rank > 1L) {
    rhs <- call("array", rhs, generate_r_dim(data_info, rewrite))
  }
  call("<-", lhs, rhs)
}


generate_r_equation_alloc_interpolate <- function(eq, data_info,
                                                    dat, rewrite) {
  name_target <- eq$interpolate$equation
  name_arg <- eq$interpolate$y

  data <- dat$data
  data_info_target <- data$elements[[name_target]]
  data_info_t <- data$elements[[eq$interpolate$t]]
  data_info_arg <- data$elements[[eq$interpolate$y]]

  dim_arg <- generate_r_dim(data_info_arg, rewrite)

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

  check <- call(dat$meta$support$check_interpolate_y,
                dim_arg, dim_target, name_arg, name_target)

  lhs <- rewrite(eq$lhs)
  args <- list(quote(cinterpolate::interpolation_function),
               rewrite(eq$interpolate$t),
               rewrite(eq$interpolate$y),
               eq$interpolate$type,
               scalar = TRUE,
               fail_on_extrapolate = TRUE)
  rhs <- as.call(args)
  list(check, call("<-", lhs, rhs))
}


generate_r_equation_alloc_ring <- function(eq, data_info, dat,
                                             rewrite) {
  data_info_contents <- dat$data$elements[[eq$delay]]

  lhs <- rewrite(eq$lhs)

  ## TODO: need to get n_history into here - follow same approach as
  ## use_dde I think.  However, it's a little more complex because it
  ## needs to be done on *create* and set into the internal data quite
  ## early.
  n_history <- DEFAULT_HISTORY_SIZE
  if (data_info_contents$rank == 0L) {
    len <- 1L
  } else {
    len <- rewrite(data_info_contents$dimnames$length)
  }
  args <- list(quote(ring::ring_buffer_bytes_typed),
               n_history, "double", len, "overwrite")
  rhs <- as.call(args)

  call("<-", lhs, rhs)
}


generate_r_equation_copy <- function(eq, data_info, dat, rewrite) {
  ## NOTE: this applies only to copying a variable into the output
  offset <- rewrite(dat$data$output$contents[[eq$lhs]]$offset)
  storage <- as.name(dat$meta$output)

  if (data_info$rank == 0) {
    lhs <- call("[[", storage, r_offset_to_position(offset))
  } else{
    i <- call("seq_len", rewrite(data_info$dimnames$length))
    lhs <- call("[", storage, call("+", offset, i))
  }

  rhs <- rewrite(eq$lhs)
  call("<-", lhs, rhs)
}


generate_r_equation_interpolate <- function(eq, data_info, dat, rewrite) {
  rhs <- as.call(list(rewrite(eq$interpolate), as.name(dat$meta$time)))
  call("<-", rewrite(eq$lhs), rhs)
}


## NOTE: There are two entirely separate codepaths here so this could
## be factored out again (and probably should be).
generate_r_equation_user <- function(eq, data_info, dat, rewrite) {
  user <- as.name(dat$meta$user)
  internal <- as.name(dat$meta$internal)

  min <- rewrite(eq$user$min)
  max <- rewrite(eq$user$max)
  integer <- data_info$storage_type == "int"

  if (eq$user$dim) {
    len <- data_info$dimnames$length
    if (data_info$rank == 1L) {
      dims <- NULL
    } else {
      ## NOTE: passing *names* in, not rewritten expressions
      dims <- as.call(c(list(quote(c)), data_info$dimnames$dim))
    }
    call(dat$meta$support$get_user_dim, user, internal, eq$lhs, len, dims,
         min, max, integer)
  } else {
    lhs <- rewrite(eq$lhs)
    rank <- data_info$rank
    default <- rewrite(eq$user$default)
    size <- generate_r_dim(data_info, rewrite)
    rhs <- call(dat$meta$support$get_user_double,
                user, eq$lhs, internal, size, default, min, max, integer)
    call("<-", lhs, rhs)
  }
}


## TODO: allocate delay state here too
generate_r_equation_delay_index <- function(eq, data_info, dat,
                                              rewrite) {
  delay <- dat$equations[[eq$delay]]$delay
  lhs <- rewrite(eq$lhs)
  alloc <- call("<-", lhs, call("integer", rewrite(delay$variables$length)))

  index1 <- function(v) {
    d <- dat$data$elements[[v$name]]
    offset <- dat$data$variable$contents[[v$name]]$offset
    if (d$rank == 0L) {
      call("<-",
           call("[[", lhs, r_offset_to_position(v$offset)),
           r_offset_to_position(offset))
    } else {
      seq <- call("seq_len", rewrite(d$dimnames$length))
      call("<-",
           call("[", lhs, call("+", rewrite(v$offset), seq)),
           call("+", rewrite(offset), seq))
    }
  }

  index <- unname(lapply(delay$variables$contents, index1))
  c(alloc, index)
}


generate_r_equation_delay_continuous <- function(eq, data_info, dat, rewrite) {
  delay <- eq$delay
  time <- as.name(dat$meta$time)

  initial_time <- rewrite(dat$meta$initial_time)
  state <- rewrite(delay$state)
  index <- rewrite(delay$index)

  time_set <- call("<-", time, call("-", time, rewrite(delay$time)))

  lookup_vars <- r_expr_if(
    rewrite(dat$meta$use_dde),
    call("<-", state, as.call(c(quote(dde::ylag), time, index))),
    call("<-", state, as.call(c(quote(deSolve::lagvalue), time, index))))
  unpack_vars <- lapply(delay$variables$contents,
                        r_unpack_variable, dat$data$elements, state, rewrite)

  eqs_src <- ir_substitute(dat$equations[delay$equations], delay$substitutions)
  eqs <- r_flatten_eqs(lapply(eqs_src, generate_r_equation,
                            dat, rewrite))

  ## Only used where there is no default:
  unpack_initial <-
    lapply(dat$data$variable$contents[names(delay$variables$contents)],
           function(x) call("<-", as.name(x$name), rewrite(x$initial)))
  unpack <- r_expr_if(call("<=", time, initial_time),
                      unpack_initial, c(lookup_vars, unpack_vars))

  rhs_expr <- ir_substitute_sexpr(eq$rhs$value, delay$substitutions)
  if (data_info$rank == 0L) {
    lhs <- rewrite(eq$lhs)
    rhs <- rewrite(rhs_expr)
    if (is.null(delay$default)) {
      body <- r_expr_local(c(time_set, unpack, eqs, rhs))
      ret <- call("<-", lhs, body)
    } else {
      default <- rewrite(delay$default)
      body <- r_expr_local(list(
        time_set,
        r_expr_if(
          call("<=", time, initial_time),
          default,
          c(lookup_vars, unpack_vars, eqs, rhs))))
      ret <- call("<-", lhs, body)
    }
  } else {
    ## TODO: generating the lhs by hand because
    ## 'generate_r_equation_array_lhs' assumes things about
    ## expressions that are not correct here.
    index <- lapply(eq$rhs$index, function(x) as.name(x$index))
    lhs <- as.call(c(list(quote(`[`), rewrite(data_info$name)), index))
    expr <- generate_r_equation_array_rhs(
      rhs_expr, eq$rhs$index, lhs, rewrite)
    if (is.null(delay$default)) {
      ret <- r_expr_local(c(time_set, unpack, eqs, expr))
    } else {
      default <- generate_r_equation_array_rhs(
        delay$default, eq$rhs$index, lhs, rewrite)
      ret <- r_expr_local(list(
        time_set,
        r_expr_if(
          call("<=", time, initial_time),
          default,
          c(lookup_vars, unpack_vars, eqs, expr))))
    }
  }

  ret
}


generate_r_equation_delay_discrete <- function(eq, data_info, dat, rewrite) {
  ring <- rewrite(eq$delay$ring)
  lhs <- rewrite(eq$lhs)
  if (data_info$rank == 0L) {
    push <- as.call(list(call("$", ring, quote(push)), rewrite(eq$rhs$value)))
  } else {
    ## The "best" scratch space here will vary - in the C version
    ## we'll use the head directly, then later on swap a const pointer
    ## in for the data.
    index <- lapply(eq$rhs$index, function(x) as.name(x$index))
    lhs_i <- as.call(c(list(quote(`[`), rewrite(eq$lhs)), index))
    push <- r_expr_local(list(
      generate_r_equation_array_rhs(
        eq$rhs$value, eq$rhs$index, lhs_i, rewrite),
      as.call(list(call("$", ring, quote(push)), lhs))))
  }

  dim <- generate_r_dim(data_info, rewrite)
  read_rhs <- as.call(list(call("$", ring, quote(head_offset)),
                           rewrite(eq$delay$time)))
  if (data_info$rank > 1L) {
    read_rhs <- call("array", read_rhs, dim)
  }
  read <- call("<-", lhs, read_rhs)

  if (is.null(eq$delay$default)) {
    default_rhs <- as.call(list(call("$", ring, quote(tail))))
    if (data_info$rank > 1L) {
      default_rhs <- call("array", default_rhs, dim)
    }
    default <- call("<-", lhs, default_rhs)
  } else {
    if (data_info$rank == 0L) {
      default <- call("<-", lhs, rewrite(eq$delay$default))
    } else {
      default <- generate_r_equation_array_rhs(
        eq$delay$default, eq$rhs$index, lhs_i, rewrite)
    }
  }

  time_check <- call(
    "<",
    call("(", call("-", rewrite(dat$meta$time), rewrite(eq$delay$time))),
    rewrite(dat$meta$initial_time))

  list(push, r_expr_if(time_check, default, read))
}


## For internal storage we can do:
## > STORAGE[[NAME]][i, j]
## but the variable/output case is different as it's
## > STORAGE[OFFSET + f(i, j)]
##
## In C we'll do this as
## > STORAGE->NAME[f(i, j)]
## and
## > STORAGE[OFFSET + f(i, j)]
generate_r_equation_array_lhs <- function(eq, data_info, dat, rewrite) {
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
    pos <- r_fold_call("+", lapply(seq_len(data_info$rank), f))
    offset <- rewrite(dat$data[[location]]$contents[[data_info$name]]$offset)
    storage <- if (location == "variable") dat$meta$result else dat$meta$output
    lhs <- call("[[", as.name(storage), call("+", offset, pos))
  }

  lhs
}


generate_r_equation_array_rhs <- function(value, index, lhs, rewrite) {
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
