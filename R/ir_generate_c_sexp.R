generate_c_sexp <- function(x, data, meta) {
  if (is.recursive(x)) {
    fn <- x[[1L]]
    args <- x[-1L]
    n <- length(args)
    values <- vcapply(args, generate_c_sexp, data, meta)

    if (fn == "(") {
      ret <- sprintf("(%s)", values[[1]])
    } else if (fn == "[") {
      if (is.numeric(args[[2L]])) {
        index <- generate_c_sexp(args[[2L]] - 1L, data, meta)
      } else {
        index <- sprintf("%s - 1", values[[2L]])
      }
      ret <- sprintf("%s[%s]", values[[1]], index)
    } else if (n == 2L && fn %in% FUNCTIONS_INFIX) {
      fmt <- switch(fn,
                    "/" = "%s %s (double) %s",
                    "^" = "%s%s%s",
                    "%s %s %s")
      ret <- sprintf(fmt, values[[1]], fn, values[[2]])
    } else if (fn == "length") {
      ret <- generate_c_sexp(data$elements[[args[[1L]]]]$dimnames$length,
                             data, meta)
    } else {
      ret <- sprintf("%s(%s)", fn, paste(values, collapse = ", "))
    }
    ret
  } else if (is.character(x)) {
    location <- data$elements[[x]]$location
    if (!is.null(location) && location == "internal") {
      sprintf("%s->%s", meta$internal, x)
    } else {
      x
    }
  } else if (is.numeric(x)) {
    deparse(x, control = "digits17")
  }
}
