generate_c_sexp <- function(x, data, meta) {
  if (is.recursive(x)) {
    fn <- x[[1L]]
    args <- x[-1L]
    n <- length(args)
    values <- vcapply(args, generate_c_sexp, data, meta)

    if (fn == "(") {
      ret <- sprintf("(%s)", values[[1]])
    } else if (fn == "[") {
      pos <- c_array_access(args[[1L]], args[-1], data, meta)
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
    } else if (fn == "sum" || fn == "odin_sum") {
      ret <- generate_c_sexp_sum(args, data, meta)
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


generate_c_sexp_sum <- function(args, data, meta) {
  target <- generate_c_sexp(args[[1]], data, meta)
  data_info <- data$elements[[args[[1]]]]
  if (length(args) == 1L) {
    len <- generate_c_sexp(data_info$dimnames$length, data, meta)
    sprintf("odin_sum1(%s, 0, %s - 1)", target, len)
  } else {
    stop("writeme")
    ## message(odin_generate2_sum(2)$definitions)
    n <- (length(args) - 1L) / 2
    sprintf("odin_sum%d(%s, %s)", n, target, args_str)
  }
}


c_fold_call <- function(fn, args) {
  if (length(args) == 1L) {
    args[[1L]]
  } else {
    sprintf("%s(%s, %s)", fn, args[[1L]], c_fold_call(fn, args[-1]))
  }
}


## See: generate_r_equation_array_lhs
c_array_access <- function(target, index, data, meta) {
  rewrite <- function(x) {
    generate_c_sexp(x, data, meta)
  }

  minus1 <- function(x, protect = TRUE) {
    if (is.numeric(x)) {
      x - 1L
    } else {
      sprintf(if (protect) "(%s - 1)" else "%s - 1", rewrite(x))
    }
  }

  mult <- data$elements[[target]]$dimnames$mult

  f <- function(i) {
    if (i == 1) {
      minus1(index[[i]], FALSE)
    } else {
      sprintf("%s * %s", rewrite(mult[[i]]), minus1(index[[i]]))
    }
  }

  paste(vcapply(rev(seq_along(index)), f), collapse = " + ")
}
