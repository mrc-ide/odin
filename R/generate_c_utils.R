c_variable_reference <- function(x, data_info, state, rewrite) {
  if (data_info$rank == 0L) {
    sprintf("%s[%s]", state, rewrite(x$offset))
  } else {
    sprintf("%s + %s", state, rewrite(x$offset))
  }
}


c_flatten_eqs <- function(eqs) {
  unlist(unname(eqs))
}


c_function <- function(return_type, name, args, body) {
  args_str <- paste(sprintf_safe("%s %s", names(args), unname(args)),
                    collapse = ", ")
  str <- sprintf_safe("%s %s(%s)", return_type, name, args_str)
  list(name = name,
       declaration = paste0(str, ";"),
       definition = c(paste0(str, " {"), paste0("  ", body), "}"))
}


c_unpack_variable <- function(name, dat, state, rewrite) {
  el <- dat$data$variable$contents[[name]]
  data_info <- dat$data$elements[[el$name]]
  rhs <- c_variable_reference(el, data_info, state, rewrite)
  if (data_info$rank == 0L) {
    fmt <- "%s %s = %s;"
  } else {
    fmt <- "%s * %s = %s;"
  }
  sprintf(fmt, data_info$storage_type, el$name, rhs)
}


## TODO: harmoise with the above - mostly this is rewriting previous uses
c_unpack_variable2 <- function(x, data_elements, state, rewrite) {
  rhs <- c_extract_variable(x, data_elements, state, rewrite)
  sprintf_safe("%s = %s;", x$name, rhs)
}


## TODO: this is the same as c_variable_reference
c_extract_variable <- function(x, data_elements, state, rewrite) {
  d <- data_elements[[x$name]]
  if (d$rank == 0L) {
    sprintf("%s[%s]", state, rewrite(x$offset))
  } else {
    sprintf("%s + %s", state, rewrite(x$offset))
  }
}


c_type_info <- function(storage_type) {
  if (storage_type == "double") {
    sexp_name <- "REALSXP"
    sexp_access <- "REAL"
    scalar_allocate <- "ScalarReal"
  } else if (storage_type == "int") {
    sexp_name <- "INTSXP"
    sexp_access <- "INTEGER"
    scalar_allocate <- "ScalarInteger"
  } else if (storage_type == "bool") {
    sexp_name <- "LGLSXP"
    sexp_access <- "INTEGER"
    scalar_allocate <- "ScalarLogical"
  } else {
    stop(sprintf("Invalid type %s [odin bug]", storage_type)) # nocov
  }
  list(c_name = storage_type,
       sexp_name = sexp_name,
       sexp_access = sexp_access,
       scalar_allocate = scalar_allocate)
}


c_expr_if <- function(condition, a, b = NULL) {
  if (is.null(b)) {
    c(sprintf_safe("if (%s) {", condition),
      paste0("  ", c_flatten_eqs(a)),
      "}")
  } else {
    c(sprintf_safe("if (%s) {", condition),
      paste0("  ", c_flatten_eqs(a)),
      "} else {",
      paste0("  ", c_flatten_eqs(b)),
      "}")
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
c_array_access <- function(target, index, data, meta, supported) {
  mult <- data$elements[[target]]$dimnames$mult

  f <- function(i) {
    index_i <- c_minus_1(index[[i]], i > 1, data, meta, supported)
    if (i == 1) {
      index_i
    } else {
      mult_i <- generate_c_sexp(mult[[i]], data, meta, supported)
      sprintf("%s * %s", mult_i, index_i)
    }
  }

  paste(vcapply(rev(seq_along(index)), f), collapse = " + ")
}


c_minus_1 <- function(x, protect, data, meta, supported) {
  if (is.numeric(x)) {
    generate_c_sexp(x - 1L, data, meta, supported)
  } else {
    x_expr <- generate_c_sexp(x, data, meta, supported)
    sprintf(if (protect) "(%s - 1)" else "%s - 1", x_expr)
  }
}
