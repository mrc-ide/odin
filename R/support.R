make_names2 <- function(variable_order, output_order, discrete) {
  make_names(c(variable_order, output_order), discrete)
}


support_n_out <- function(output_order) {
  n <- vnapply(output_order, function(x) if (is.null(x)) 1L else prod(x))
  as.integer(sum(n))
}


support_transform_variables <- function(y, private) {
  ord <- list(variable_order = private$variable_order,
              output_order = private$output_order)
  make_transform_variables(ord)(y)
}


support_check_user <- function(user, allowed) {
  given <- names(user)
  if (length(user) > 0 && (is.null(given) || !all(nzchar(given)))) {
    stop("All user parameters must be named", call. = FALSE)
  }
  err <- unique(given[duplicated(given)])
  if (length(err) > 0L) {
    stop("Duplicated user parameters: ", paste(err, collapse = ", "),
         call. = FALSE)
  }
  err <- setdiff(given, allowed)
  if (length(err) > 0L) {
    stop("Unknown user parameters: ", paste(err, collapse = ", "),
         call. = FALSE)
  }
}


as_integer <- function(x, name = deparse(substitute(x))) {
  if (is.integer(x)) {
    x
  } else if (is.numeric(x)) {
    ret <- as.integer(x)
    if (max(abs(ret - x)) > sqrt(.Machine$double.eps)) {
      stop(sprintf("Expected integer input for '%s'", name), call. = FALSE)
    }
    ret
  } else {
    stop(sprintf("Expected integer input for '%s'", name), call. = FALSE)
  }
}


as_numeric <- function(x, name = deparse(substitute(x))) {
  if (is.integer(x)) {
    as.numeric(x)
  } else if (is.numeric(x)) {
    x
  } else {
    stop(sprintf("Expected numeric input for '%s'", name), call. = FALSE)
  }
}
