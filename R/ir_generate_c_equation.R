generate_c_equations <- function(dat, rewrite) {
  lapply(dat$equations, generate_c_equation, dat, rewrite)
}


generate_c_equation <- function(eq, dat, rewrite) {
  f <- switch(
    eq$type,
    expression_scalar = generate_c_equation_scalar,
    expression_array = generate_c_equation_array,
    alloc = generate_c_equation_alloc,
    alloc_interpolate = generate_c_equation_alloc_interpolate,
    alloc_ring = generate_c_equation_alloc_ring,
    copy = generate_c_equation_copy,
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
  c(sprintf_safe("Free(%s);", lhs),
    sprintf_safe("%s = (%s*) Calloc(%s, %s);", lhs, ctype, len, ctype))
}


generate_c_equation_alloc_interpolate <- function(eq, data_info, dat, rewrite) {
  .NotYetImplemented()
}


generate_c_equation_alloc_ring <- function(eq, data_info, dat, rewrite) {
  .NotYetImplemented()
}


generate_c_equation_copy <- function(eq, data_info, dat, rewrite) {
  x <- dat$data$output$contents[[data_info$name]]
  target <- c_variable_reference(x, data_info, "output", rewrite)
  if (data_info$rank == 0L) {
    sprintf_safe("%s = %s;", target, rewrite(eq$lhs))
  } else {
    sprintf_safe("memcpy(%s, %s, %s * sizeof(%s));",
                 target, rewrite(eq$lhs), rewrite(data_info$dimnames$length),
                 data_info$storage_type)
  }
}


generate_c_equation_user <- function(eq, data_info, dat, rewrite) {
  min <- rewrite(eq$user$min)
  max <- rewrite(eq$user$max)
  integer <- data_info$storage_type == "int"

  user <- dat$meta$user
  rank <- data_info$rank

  lhs <- rewrite(eq$lhs)
  storage_type <- data_info$storage_type
  is_integer <- if (storage_type == "int") "true" else "false"
  if (eq$user$dim) {
    free <- sprintf_safe("Free(%s);", lhs)
    len <- data_info$dimnames$length

    if (rank == 1L) {
      ret <- c(
        free,
        sprintf_safe('%s = (%s*) get_user_array_dim(%s, %s, "%s", %d, &%s);',
                     lhs, storage_type, user, is_integer, eq$lhs, rank,
                     rewrite(len)))
    } else {
      ret <- c(
        free,
        sprintf_safe("int %s[%d];", len, rank + 1),
        sprintf_safe('%s = (%s*) get_user_array_dim(%s, %s, "%s", %d, %s);',
                     lhs, storage_type, user, is_integer, eq$lhs, rank, len),
        sprintf_safe("%s = %s[%d];", rewrite(len), len, 0),
        sprintf_safe("%s = %s[%d];",
                     vcapply(data_info$dimnames$dim, rewrite), len,
                     seq_len(rank)))
    }
  } else {
    if (rank == 0L) {
      ret <- sprintf_safe(
        '%s = get_user_%s(%s, "%s", %s);',
        lhs, data_info$storage_type, user, eq$lhs, lhs)
    } else {
      if (rank == 1L) {
        dim <- rewrite(data_info$dimnames$length)
      } else {
        dim <- paste(vcapply(data_info$dimnames$dim, rewrite), collapse = ", ")
      }
      ret <- c(sprintf_safe("Free(%s);", lhs),
               sprintf_safe('%s = (%s*) get_user_array(%s, %s, "%s", %d, %s);',
                            lhs, storage_type, user, is_integer, eq$lhs,
                            rank, dim))
    }
  }
  ret
}


generate_c_equation_delay_index <- function(eq, data_info, dat, rewrite) {
  delay <- dat$equations[[eq$delay]]$delay
  lhs <- rewrite(eq$lhs)

  alloc <- c(sprintf_safe("Free(%s);", lhs),
             sprintf_safe("%s = Calloc(%s, int);",
                          lhs, rewrite(delay$variables$length)))

  index1 <- function(v) {
    d <- dat$data$elements[[v$name]]
    offset <- dat$data$variable$contents[[v$name]]$offset
    if (d$rank == 0L) {
      sprintf_safe("%s[%s] = %s;", lhs, v$offset, offset)
    } else {
      stop("checkme")
      loop <- sprintf_safe(
        "for (size_t i = %s, j = %s; j < %s; ++i, ++j)",
        rewrite(v$offset), rewrite(offset), rewrite(d$dimnames$length))
      c(loop, "  lhs[i] <- j;", "}")
    }
  }

  index <- c_flatten_eqs(lapply(delay$variables$contents, index1))
  c(alloc, index)
}


generate_c_equation_delay_continuous <- function(eq, data_info, dat, rewrite) {
  delay <- eq$delay
  time <- dat$meta$time

  initial_time <- rewrite(dat$meta$initial_time)
  state <- rewrite(delay$state)
  index <- rewrite(delay$index)
  len <- rewrite(delay$variables$length)

  time_set <- sprintf("double %s = %s - %s;", time, time, rewrite(delay$time))

  lookup_vars <- c_expr_if(
    rewrite(dat$meta$use_dde),
    sprintf_safe("lagvalue_dde(%s, %s, %s, %s);",
                 time, index, len, state),
    sprintf_safe("lagvalue_ds(%s, %s, %s, %s);",
                 time, index, len, state))

  unpack_vars <- c_flatten_eqs(lapply(
    delay$variables$contents, c_unpack_variable2,
    dat$data$elements, state, FALSE, rewrite))

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

  ## Only used where there is no default:
  unpack_initial <-
    lapply(dat$data$variable$contents[names(delay$variables$contents)],
           unpack_initial1)
  unpack <- c(decl,
              c_expr_if(
                sprintf_safe("%s <= %s", time, initial_time),
                c_flatten_eqs(unpack_initial),
                c(lookup_vars, unpack_vars)))

  if (data_info$rank != 0L) {
    stop("checkme")
  }
  if (!is.null(delay$default)) {
    stop("checkme")
  }
  if (data_info$location != "transient") {
    stop("checkme")
  }

  rhs_expr <- ir_substitute_sexpr(eq$rhs$value, delay$substitutions)
  exit <- sprintf_safe("%s = %s;", rewrite(eq$lhs), rewrite(rhs_expr))

  ret <- c(sprintf_safe("%s %s;", data_info$storage_type, eq$lhs),
           "{",
           paste0("  ", c(time_set, unpack, eqs, exit)),
           "}")

  ret
}


generate_c_equation_delay_discrete <- function(eq, data_info, dat, rewrite) {
  .NotYetImplemented()
}


generate_c_equation_array_lhs <- function(eq, data_info, dat, rewrite) {
  index <- vcapply(eq$rhs[[1]]$index, "[[", "index")
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
  if (!seen_range) {
    ret <- c("{", paste("  ", ret), "}")
  }
  ret
}


c_expr_if <- function(condition, a, b) {
  c(sprintf_safe("if (%s) {", condition),
    paste0("  ", c_flatten_eqs(a)),
    "} else {",
    paste0("  ", c_flatten_eqs(b)),
    "}")
}
