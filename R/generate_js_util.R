js_flatten_eqs <- function(eqs) {
  unlist(unname(eqs))
}


js_function <- function(args, body, name = NULL) {
  if (is.null(name)) {
    start <- sprintf("function(%s) {", paste(args, collapse = ", "))
  } else {
    start <- sprintf("function %s(%s) {", name, paste(args, collapse = ", "))
  }
  if (length(body) > 0L) {
    body <- paste0("  ", body)
  }
  c(start, body, "}")
}


js_extract_variable <- function(x, data_elements, state, rewrite) {
  d <- data_elements[[x$name]]
  if (d$rank == 0L) {
    sprintf("%s[%s]", state, rewrite(x$offset))
  } else {
    offset <- rewrite(x$offset)
    len <- rewrite(d$dimnames$length)
    sprintf("%s.slice(%s, %s + %s)", state, offset, offset, len)
  }
}


js_unpack_variable <- function(name, dat, state, rewrite) {
  x <- dat$data$variable$contents[[name]]
  rhs <- js_extract_variable(x, dat$data$elements, state, rewrite)
  sprintf("var %s = %s;", x$name, rhs)
}


js_array_access <- function(target, index, data, meta) {
  mult <- data$elements[[target]]$dimnames$mult

  f <- function(i) {
    index_i <- js_minus_1(index[[i]], i > 1, data, meta)
    if (i == 1) {
      index_i
    } else {
      mult_i <- generate_js_sexp(mult[[i]], data, meta)
      sprintf("%s * %s", mult_i, index_i)
    }
  }

  paste(vcapply(rev(seq_along(index)), f), collapse = " + ")
}


js_minus_1 <- function(x, protect, data, meta) {
  if (is.numeric(x)) {
    generate_js_sexp(x - 1L, data, meta)
  } else {
    x_expr <- generate_js_sexp(x, data, meta)
    sprintf(if (protect) "(%s - 1)" else "%s - 1", x_expr)
  }
}


js_fold_call <- function(fn, args) {
  if (length(args) == 1L) {
    args[[1L]]
  } else {
    sprintf("%s(%s, %s)", fn, args[[1L]], js_fold_call(fn, args[-1]))
  }
}
