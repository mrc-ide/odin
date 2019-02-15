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
  .NotYetImplemented()
}


generate_c_equation_alloc <- function(eq, data_info, dat, rewrite) {
  .NotYetImplemented()
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
  .NotYetImplemented()
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
