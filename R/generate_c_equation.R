generate_c_equations <- function(dat, rewrite) {
  lapply(dat$equations, generate_c_equation, dat, rewrite)
}


generate_c_equation <- function(eq, dat, rewrite) {
  f <- switch(
    eq$type,
    expression_scalar = generate_c_equation_scalar,
    expression_inplace = generate_c_equation_inplace,
    expression_array = generate_c_equation_array,
    alloc = generate_c_equation_alloc,
    alloc_interpolate = generate_c_equation_alloc_interpolate,
    alloc_ring = generate_c_equation_alloc_ring,
    copy = generate_c_equation_copy,
    interpolate = generate_c_equation_interpolate,
    user = generate_c_equation_user,
    delay_index = generate_c_equation_delay_index,
    delay_continuous = generate_c_equation_delay_continuous,
    delay_discrete = generate_c_equation_delay_discrete,
    stop("Unknown type"))

  data_info <- dat$data$elements[[eq$lhs]]
  stopifnot(!is.null(data_info))

  f(eq, data_info, dat, rewrite)
}


generate_c_equation_scalar <- function(eq, data_info, dat, rewrite) {
  location <- data_info$location
  if (location == "transient") {
    lhs <- sprintf("%s %s", data_info$storage_type, eq$lhs)
  } else if (location == "internal") {
    lhs <- rewrite(eq$lhs)
  } else {
    offset <- dat$data[[location]]$contents[[data_info$name]]$offset
    storage <- if (location == "variable") dat$meta$result else dat$meta$output
    lhs <- sprintf("%s[%s]", storage, rewrite(offset))
  }
  rhs <- rewrite(eq$rhs$value)
  sprintf("%s = %s;", lhs, rhs)
}


generate_c_equation_inplace <- function(eq, data_info, dat, rewrite) {
  location <- data_info$location
  lhs <- rewrite(eq$lhs)
  fn <- eq$rhs$value[[1]]
  args <- lapply(eq$rhs$value[-1], rewrite)
  switch(
    fn,
    rmultinom = generate_c_equation_inplace_rmultinom(eq, lhs, dat, rewrite),
    rmhyper = generate_c_equation_inplace_rmhyper(
      eq, lhs, data_info, dat, rewrite),
    stop("unhandled array expression [odin bug]")) # nocov
}


generate_c_equation_inplace_rmultinom <- function(eq, lhs, dat, rewrite) {
  args <- eq$rhs$value[-1]
  len <- rewrite(dat$data$elements[[args[[2]]]]$dimnames$length)
  stopifnot(!is.null(len))
  sprintf_safe("Rf_rmultinom(%s, %s, %s, %s);",
               rewrite(args[[1]]), rewrite(args[[2]]), len, lhs)
}


generate_c_equation_inplace_rmhyper <- function(eq, lhs, data_info, dat,
                                                rewrite) {
  len <- data_info$dimnames$length
  n <- eq$rhs$value[[2]]
  src <- eq$rhs$value[[3]]
  src_type <- dat$data$elements[[src]]$storage_type
  fn <- if (src_type == "int") "rmhyper_i" else "rmhyper_d"
  sprintf_safe("%s(%s, %s, %s, %s);",
               fn, rewrite(n), rewrite(src), rewrite(len), lhs)
}


generate_c_equation_array <- function(eq, data_info, dat, rewrite) {
  lhs <- generate_c_equation_array_lhs(eq, data_info, dat, rewrite)
  lapply(eq$rhs, function(x)
    generate_c_equation_array_rhs(x$value, x$index, lhs, rewrite))
}


generate_c_equation_alloc <- function(eq, data_info, dat, rewrite) {
  lhs <- rewrite(eq$lhs)
  ctype <- data_info$storage_type
  len <- rewrite(data_info$dimnames$length)

  ## TODO: this is fine for now, but if we know that some variables
  ## have constant size, then we do not have to ever free them.  It's
  ## fairly harmless though.
  c(sprintf_safe("R_Free(%s);", lhs),
    sprintf_safe("%s = (%s*) R_Calloc(%s, %s);", lhs, ctype, len, ctype))
}


## TODO: once we get proper array checking, all the interpolation
## length bits will move into that - we'll generate out some lists of
## constraints but these will have a "must be equal" rather than "must
## be at least".  That will be required for things like matrix
## multiplication too.
generate_c_equation_alloc_interpolate <- function(eq, data_info, dat, rewrite) {
  data_info_target <- dat$data$elements[[eq$interpolate$equation]]
  data_info_t <- dat$data$elements[[eq$interpolate$t]]
  data_info_y <- dat$data$elements[[eq$interpolate$y]]

  ## TOOD: There is some key checking that we're currently missing here
  len_t <- rewrite(data_info_t$dimnames$length)

  lhs <- rewrite(eq$lhs)

  if (data_info_target$rank == 0L) {
    len_result <- rewrite(1L)
    len_y <- rewrite(data_info_y$dimnames$length)
    check <- sprintf_safe(
      'interpolate_check_y(%s, %s, 0, "%s", "%s");',
      len_t, len_y, data_info_y$name, eq$interpolate$equation)
  } else {
    len_result <- rewrite(data_info_target$dimnames$length)
    rank <- data_info_target$rank
    len_y <- vcapply(data_info_y$dimnames$dim, rewrite)
    i <- seq_len(rank + 1)
    if (rank == 1L) {
      len_expected <- c(len_t, rewrite(data_info_target$dimnames$length))
    } else {
      len_expected <- c(
        len_t,
        vcapply(data_info_target$dimnames$dim[seq_len(rank)], rewrite))
    }
    check <- sprintf_safe(
      'interpolate_check_y(%s, %s, %d, "%s", "%s");',
      len_expected, len_y, seq_len(rank + 1), data_info_y$name,
      eq$interpolate$equation)
  }

  rhs <- sprintf_safe(
    'cinterpolate_alloc("%s", %s, %s, %s, %s, true, false)',
    eq$interpolate$type, len_t, len_result, rewrite(eq$interpolate$t),
    rewrite(eq$interpolate$y))

  c(check,
    sprintf_safe("cinterpolate_free(%s);", lhs),
    sprintf_safe("%s = %s;", lhs, rhs))
}


generate_c_equation_interpolate <- function(eq, data_info, dat, rewrite) {
  if (data_info$rank == 0L) {
    lhs <- rewrite(eq$lhs)
    ret <- sprintf_safe("cinterpolate_eval(%s, %s, &%s);",
                        dat$meta$time, rewrite(eq$interpolate),
                        rewrite(eq$lhs))
    if (data_info$location == "transient") {
      ret <- c(sprintf_safe("double %s = 0.0;", eq$lhs), ret)
    }
  } else {
    ret <- sprintf_safe("cinterpolate_eval(%s, %s, %s);",
                        dat$meta$time, rewrite(eq$interpolate),
                        rewrite(eq$lhs))
  }
  ret
}


generate_c_equation_alloc_ring <- function(eq, data_info, dat, rewrite) {
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

  c(sprintf_safe("if (%s) {", lhs),
    sprintf_safe("  ring_buffer_destroy(%s);", lhs),
    sprintf_safe("}"),
    sprintf_safe("%s = ring_buffer_create(%s, %s * sizeof(double), %s);",
                 lhs, n_history, len, "OVERFLOW_OVERWRITE"))
}


generate_c_equation_copy <- function(eq, data_info, dat, rewrite) {
  x <- dat$data$output$contents[[data_info$name]]
  target <- c_variable_reference(x, data_info, "output", rewrite)
  if (data_info$rank == 0L) {
    sprintf_safe("%s = %s;", target, rewrite(eq$lhs))
  } else {
    len <- rewrite(data_info$dimnames$length)
    lhs <- rewrite(eq$lhs)
    if (data_info$storage_type == "double") {
      sprintf_safe("memcpy(%s, %s, %s * sizeof(%s));",
                   target, lhs, len, data_info$storage_type)
    } else {
      offset <- rewrite(x$offset)
      c(sprintf_safe("for (int i = 0; i < %s; ++i) {", len),
        sprintf_safe("  output[%s + i] = %s[i];", offset, lhs),
        sprintf_safe("}"))
    }
  }
}


generate_c_equation_user <- function(eq, data_info, dat, rewrite) {
  user <- dat$meta$user
  rank <- data_info$rank

  lhs <- rewrite(eq$lhs)
  storage_type <- data_info$storage_type
  is_integer <- if (storage_type == "int") "true" else "false"
  ## TODO: add things like NA_REAL to reserved words
  min <- rewrite(eq$user$min %||% "NA_REAL")
  max <- rewrite(eq$user$max %||% "NA_REAL")
  previous <- lhs

  if (eq$user$dim) {
    free <- sprintf_safe("R_Free(%s);", lhs)
    len <- data_info$dimnames$length
    if (rank == 1L) {
      ret <-
        sprintf_safe(
          '%s = (%s*) user_get_array_dim(%s, %s, %s, "%s", %d, %s, %s, &%s);',
          lhs, storage_type, user, is_integer, previous, eq$lhs, rank, min, max,
          rewrite(len))
    } else {
      ret <- c(
        sprintf_safe("int %s[%d];", len, rank + 1),
        sprintf_safe(
          '%s = (%s*) user_get_array_dim(%s, %s, %s, "%s", %d, %s, %s, %s);',
          lhs, storage_type, user, is_integer, previous, eq$lhs, rank,
          min, max, len),
        sprintf_safe("%s = %s[%d];", rewrite(len), len, 0),
        sprintf_safe("%s = %s[%d];",
                     vcapply(data_info$dimnames$dim, rewrite), len,
                     seq_len(rank)))
    }
  } else {
    if (rank == 0L) {
      ret <- sprintf_safe(
        '%s = user_get_scalar_%s(%s, "%s", %s, %s, %s);',
        lhs, data_info$storage_type, user, eq$lhs, lhs, min, max)
    } else {
      if (rank == 1L) {
        dim <- rewrite(data_info$dimnames$length)
      } else {
        dim <- paste(vcapply(data_info$dimnames$dim, rewrite), collapse = ", ")
      }
      ret <- sprintf_safe(
        '%s = (%s*) user_get_array(%s, %s, %s, "%s", %s, %s, %d, %s);',
        lhs, storage_type, user, is_integer, previous, eq$lhs,
        min, max, rank, dim)
    }
  }
  ret
}


generate_c_equation_delay_index <- function(eq, data_info, dat, rewrite) {
  delay <- dat$equations[[eq$delay]]$delay
  lhs <- rewrite(eq$lhs)
  state <- rewrite(delay$state)

  alloc <- c(sprintf_safe("R_Free(%s);", lhs),
             sprintf_safe("%s = R_Calloc(%s, int);",
                          lhs, rewrite(delay$variables$length)),
             sprintf_safe("R_Free(%s);", state),
             sprintf_safe("%s = R_Calloc(%s, double);",
                          state, rewrite(delay$variables$length)))

  index1 <- function(v) {
    d <- dat$data$elements[[v$name]]
    offset <- dat$data$variable$contents[[v$name]]$offset
    if (d$rank == 0L) {
      sprintf_safe("%s[%s] = %s;", lhs, v$offset, offset)
    } else {
      loop <- sprintf_safe(
        "for (int i = 0, j = %s; i < %s; ++i, ++j) {",
        rewrite(offset), rewrite(d$dimnames$length))
      c(loop,
        sprintf_safe("  %s[%s + i] = j;", lhs, rewrite(v$offset)),
        "}")
    }
  }

  index <- c_flatten_eqs(lapply(delay$variables$contents, index1))
  c(alloc, index)
}


generate_c_equation_delay_continuous <- function(eq, data_info, dat, rewrite) {
  delay <- eq$delay
  time <- dat$meta$time
  time_true <- sprintf("%s_true", time)

  initial_time <- rewrite(dat$meta$initial_time)
  state <- rewrite(delay$state)
  index <- rewrite(delay$index)
  len <- rewrite(delay$variables$length)

  if (is.recursive(delay$time)) {
    dt <- rewrite(call("(", delay$time))
  } else {
    dt <- rewrite(delay$time)
  }
  time_set <- c(
    sprintf_safe("const double %s = %s;", time_true, time),
    sprintf_safe("const double %s = %s - %s;", time, time_true, dt))

  lookup_vars <- sprintf_safe(
    "lagvalue(%s, %s, %s, %s, %s);",
    time, rewrite(dat$meta$c$use_dde), index, len, state)

  unpack_vars <- c_flatten_eqs(lapply(
    delay$variables$contents, c_unpack_variable2,
    dat$data$elements, state, rewrite))

  eqs_src <- ir_substitute(dat$equations[delay$equations], delay$substitutions)
  eqs <- c_flatten_eqs(lapply(eqs_src, generate_c_equation, dat, rewrite))

  unpack_initial1 <- function(x) {
    d <- dat$data$elements[[x$name]]
    sprintf_safe("%s = %s;", x$name, rewrite(x$initial))
  }

  decl1 <- function(x) {
    d <- dat$data$elements[[x$name]]
    fmt <- if (d$rank == 0L) "%s %s;" else "%s *%s;"
    sprintf_safe(fmt, d$storage_type, x$name)
  }

  decl <- c_flatten_eqs(lapply(delay$variables$contents, decl1))

  rhs_expr <- ir_substitute_sexpr(eq$rhs$value, delay$substitutions)
  if (data_info$rank == 0L) {
    lhs <- rewrite(eq$lhs)
    expr <- sprintf_safe("%s = %s;", lhs, rewrite(rhs_expr))
  } else {
    lhs <- generate_c_equation_array_lhs(eq, data_info, dat, rewrite)
    expr <- generate_c_equation_array_rhs(rhs_expr, eq$rhs$index, lhs, rewrite)
  }

  needs_variables <- length(delay$variables$contents) > 0L
  if (is.null(delay$default)) {
    if (needs_variables) {
      unpack_initial <-
        lapply(dat$data$variable$contents[names(delay$variables$contents)],
               unpack_initial1)
      unpack <- c(decl,
                  c_expr_if(
                    sprintf_safe("%s <= %s", time, initial_time),
                    c_flatten_eqs(unpack_initial),
                    c(lookup_vars, unpack_vars)))
    } else {
      unpack <- NULL
    }
    body <- c(time_set, unpack, eqs, expr)
  } else {
    if (data_info$rank == 0L) {
      default <- sprintf_safe("%s = %s;", lhs, rewrite(delay$default))
    } else {
      default <- generate_c_equation_array_rhs(delay$default, eq$rhs$index,
                                               lhs, rewrite)
    }
    if (needs_variables) {
      unpack <- c(lookup_vars, unpack_vars)
    } else {
      unpack <- NULL
    }
    body <- c(time_set,
              c_expr_if(
                sprintf_safe("%s <= %s", time, initial_time),
                default,
                c(decl, unpack, eqs, expr)))
  }

  if (data_info$location == "transient") {
    setup <- sprintf_safe("%s %s;", data_info$storage_type, eq$lhs)
  } else {
    setup <- NULL
  }

  header <- sprintf_safe("// delay block for %s", eq$name)

  c(header, setup, "{", paste0("  ", body), "}")
}



generate_c_equation_delay_discrete <- function(eq, data_info, dat, rewrite) {
  if (!is.null(eq$delay$default)) {
    stop("Discrete delays with default not yet supported [odin bug]")
  }

  ## This can't currently be false I believe:
  stopifnot(data_info$storage_type == "double")

  head <- sprintf("%s_head", eq$delay$ring)
  tail <- sprintf("%s_tail", eq$delay$ring)
  ring <- rewrite(eq$delay$ring)
  lhs <- rewrite(eq$lhs)

  get_ring_head <- sprintf_safe(
    "double * %s = (double*) ring_buffer_head(%s);",
    head, ring)

  if (data_info$rank == 0L) {
    push <- sprintf("%s[0] = %s;", head, rewrite(eq$rhs$value))
  } else {
    data_info_ring <- data_info
    data_info_ring$name <- head
    lhs_ring <- generate_c_equation_array_lhs(eq, data_info_ring, dat, rewrite)
    push <- generate_c_equation_array_rhs(eq$rhs$value, eq$rhs$index,
                                          lhs_ring, rewrite)
  }

  advance <- sprintf_safe("ring_buffer_head_advance(%s);", ring)

  time_check <- sprintf_safe(
    "(int)%s - %s <= %s",
    dat$meta$time, rewrite(eq$delay$time), rewrite(dat$meta$initial_time))
  data_initial <- sprintf_safe(
    "%s = (double*)ring_buffer_tail(%s);", tail, ring)
  data_offset <- sprintf_safe(
    "%s = (double*) ring_buffer_head_offset(%s, %s);",
    tail, ring, rewrite(eq$delay$time))

  if (data_info$rank == 0L) {
    ## always transient, so needs declaration:
    assign <- sprintf("double %s = %s[0];", lhs, tail)
  } else {
    assign <- sprintf("memcpy(%s, %s, %s * sizeof(double));",
                      lhs, tail, rewrite(data_info$dimnames$length))
  }

  c(get_ring_head,
    push,
    advance,
    sprintf_safe("double * %s;", tail),
    c_expr_if(time_check, data_initial, data_offset),
    assign) -> ret
}


generate_c_equation_array_lhs <- function(eq, data_info, dat, rewrite) {
  if (eq$type == "expression_array") {
    index <- vcapply(eq$rhs[[1]]$index, "[[", "index")
  } else {
    index <- lapply(eq$rhs$index, "[[", "index")
  }
  location <- data_info$location

  f <- function(i) {
    if (i == 1) {
      sprintf("%s - 1", index[[i]])
    } else {
      sprintf("%s * (%s - 1)",
              rewrite(data_info$dimnames$mult[[i]]), index[[i]])
    }
  }

  pos <- paste(vcapply(seq_along(index), f), collapse = " + ")
  if (location == "internal") {
    lhs <- sprintf("%s[%s]", rewrite(data_info$name), pos)
  } else {
    offset <- rewrite(dat$data[[location]]$contents[[data_info$name]]$offset)
    storage <- if (location == "variable") dat$meta$result else dat$meta$output
    lhs <- sprintf("%s[%s + %s]", storage, offset, pos)
  }

  lhs
}


## TODO: we should really use size_t for the index variables here, but
## because the sizes are not yet stored as size_t that causes a lot of
## compiler warning noise.
generate_c_equation_array_rhs <- function(value, index, lhs, rewrite) {
  ret <- sprintf("%s = %s;", lhs, rewrite(value))
  seen_range <- FALSE
  for (idx in rev(index)) {
    if (idx$is_range) {
      seen_range <- TRUE
      loop <- sprintf_safe("for (int %s = %s; %s <= %s; ++%s) {",
                           idx$index, rewrite(idx$value[[2]]),
                           idx$index, rewrite(idx$value[[3]]),
                           idx$index)
      ret <- c(loop, paste0("  ", ret), "}")
    } else {
      ret <- c(sprintf("int %s = %s;", idx$index, rewrite(idx$value)),
               ret)
    }
  }
  if (!seen_range || !index[[1]]$is_range) {
    ret <- c("{", paste("  ", ret), "}")
  }
  ret
}
