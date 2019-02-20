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
  .NotYetImplemented()
}


generate_c_equation_user <- function(eq, data_info, dat, rewrite) {
  min <- rewrite(eq$user$min)
  max <- rewrite(eq$user$max)
  integer <- data_info$storage_type == "int"

  if (eq$user$dim) {
    stop("Implement me")
  } else {
    lhs <- rewrite(eq$lhs)
    if (data_info$rank == 0L) {
      sprintf_safe(
        '%s = get_user_%s(%s, "%s", %s);',
        lhs, data_info$storage_type, dat$meta$user, eq$lhs, lhs)
    } else {
      if (data_info$storage_type != "double") {
        stop("not yet implemented")
      }
      if (data_info$rank == 1L) {
        dim <- rewrite(data_info$dimnames$length)
      } else {
        dim <- paste(vcapply(data_info$dimnames$dim, rewrite), collapse = ", ")
      }
      c(sprintf_safe("Free(%s);", lhs),
        sprintf_safe('%s = get_user_array_double(%s, "%s", %d, %s);',
                     lhs, dat$meta$user, eq$lhs, data_info$rank, dim))
    }
  }
}


generate_c_equation_delay_index <- function(eq, data_info, dat, rewrite) {
  .NotYetImplemented()
}


generate_c_equation_delay_continuous <- function(eq, data_info, dat, rewrite) {
  .NotYetImplemented()
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
