generate_c_sexp <- function(x, data, meta, supported) {
  if (is.recursive(x)) {
    fn <- x[[1L]]
    args <- x[-1L]
    n <- length(args)
    values <- vcapply(args, generate_c_sexp, data, meta, supported)

    if (fn == "(") {
      ret <- sprintf("(%s)", values[[1]])
    } else if (fn == "[") {
      pos <- c_array_access(args[[1L]], args[-1], data, meta, supported)
      ret <- sprintf("%s[%s]", values[[1L]], pos)
    } else if (fn == "if") {
      ## NOTE: The ternary operator has very low precendence, so I'm
      ## going to agressively parenthesise it.  This is strictly not
      ## needed when this expression is the only element of `expr` but
      ## that's hard to detect so we'll tolerate a few additional
      ## parens for now.
      ret <- sprintf("(%s ? %s : %s)",
                     values[[1L]], values[[2L]], values[[3L]])
    } else if (n == 2L && fn %in% FUNCTIONS_INFIX) {
      fmt <- switch(fn,
                    "/" = "%s %s (double) %s",
                    "^" = "%s%s%s",
                    "%s %s %s")
      ret <- sprintf(fmt, values[[1]], fn, values[[2]])
    } else if (fn == "length") {
      ret <- generate_c_sexp(data$elements[[args[[1L]]]]$dimnames$length,
                             data, meta, supported)
    } else if (fn == "dim") {
      args[[1]] <- sub(sprintf("^%s->", INTERNAL), "", args[[1]])
      dim <- data$elements[[args[[1L]]]]$dimnames$dim[[args[[2]]]]
      ret <- generate_c_sexp(dim, data, meta, supported)
    } else if (fn %in% c("norm_rand", "unif_rand", "exp_rand")) {
      ret <- sprintf("%s(%s)", fn, paste(values, collapse = ", "))
    } else if (fn == "log" && length(values) == 2L) {
      ret <- sprintf("(log(%s) / log(%s))", values[[1L]], values[[2L]])
    } else if (fn == "round") {
      ## ensures same rounding behaviour of 0.5 as R:
      digits <- if (length(values) == 2L) values[[2L]] else 0
      ret <- sprintf("fround(%s, %s)", values[[1L]], digits)
    } else if (fn == "min" || fn == "max") {
      ret <- c_fold_call(paste0("f", fn), values)
    } else if (fn == "sum" || fn == "odin_sum") {
      ret <- generate_c_sexp_sum(args, data, meta, supported)
    } else if (fn == "as.integer") {
      ret <- sprintf("(int) (%s)", values[[1L]])
    } else {
      if (fn == "rbinom") {
        ## This is a little extreme but is useful in at least some
        ## cases (and I don't imagine that returning NaN will be
        ## useful most of the time).
        values[[1L]] <- sprintf("round(%s)", values[[1L]])
      }
      if (fn == "rexp") {
        values[[1L]] <- sprintf("1 / (double) %s", values[[1L]])
      }
      if (any(names(FUNCTIONS_RENAME) == fn)) {
        fn <- FUNCTIONS_RENAME[[fn]]
      } else if (any(FUNCTIONS_REWRITE_RF == fn)) {
        fn <- paste0("Rf_", fn)
      } else if (!any(c(names(FUNCTIONS), supported) == fn)) {
        stop(sprintf("unsupported function '%s' [odin bug]", fn)) # nocov
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


generate_c_sexp_sum <- function(args, data, meta, supported) {
  target <- generate_c_sexp(args[[1]], data, meta, supported)
  data_info <- data$elements[[sub(sprintf("^%s->", INTERNAL), "", args[[1]])]]
  type <- data_info$storage_type
  if (length(args) == 1L) {
    fn <- if (type == "int") "odin_isum1" else "odin_sum1"
    len <- generate_c_sexp(data_info$dimnames$length, data, meta, supported)
    sprintf("%s(%s, 0, %s)", fn, target, len)
  } else {
    if (type == "int") {
      stop("Partial integer sums not yet supported")
    }
    i <- seq(2, length(args), by = 2)

    all_args <- c(args, as.list(data_info$dimnames$mult[-1]))
    values <- character(length(all_args))
    values[i] <- vcapply(all_args[i], c_minus_1, FALSE, data, meta, supported)
    values[-i] <- vcapply(all_args[-i], generate_c_sexp, data, meta, supported)
    arg_str <- paste(values, collapse = ", ")

    sprintf_safe("odin_sum%d(%s)", length(i), arg_str)
  }
}
