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
    } else if (fn == "dim") {
      dim <- data$elements[[args[[1L]]]]$dimnames$dim[[args[[2]]]]
      ret <- generate_c_sexp(dim, data, meta)
    } else if (fn == "norm_rand") {
      ret <- sprintf("%s(%s)", fn, paste(values, collapse = ", "))
    } else if (fn == "log" && length(values) == 2L) {
      ret <- sprintf("(log(%s) / log(%s))", values[[1L]], values[[2L]])
    } else if (fn == "min" || fn == "max") {
      ret <- c_fold_call(paste0("f", fn), values)
    } else {
      if (any(names(FUNCTIONS_RENAME) == fn)) {
        fn <- FUNCTIONS_RENAME[[fn]]
      } else if (!any(names(FUNCTIONS) == fn)) {
        stop(sprintf("unsupported function '%s'", fn))
      }
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


c_fold_call <- function(fn, args) {
  if (length(args) == 1L) {
    args[[1L]]
  } else {
    sprintf("%s(%s, %s)", fn, args[[1L]], c_fold_call(fn, args[-1]))
  }
}
