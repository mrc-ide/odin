r_offset_to_position <- function(x) {
  if (is.language(x)) {
    call("+", x, 1L)
  } else {
    x + 1L
  }
}


r_fold_call <- function(fn, args) {
  ret <- args[[1L]]
  for (i in seq_along(args)[-1L]) {
    ret <- call(fn, ret, args[[i]])
  }
  ret
}


r_flatten_eqs <- function(x) {
  x <- unname(x)
  if (any(vlapply(x, is.list))) {
    x <- unlist(x, FALSE, FALSE)
  }
  x
}


r_unpack_variable <- function(x, data, state, rewrite) {
  call("<-", as.name(x$name), r_extract_variable(x, data, state, rewrite))
}


r_extract_variable <- function(x, data, state, rewrite) {
  d <- data[[x$name]]
  if (d$rank == 0L) {
    extract <- call("[[", state, r_offset_to_position(x$offset))
  } else {
    seq <- call("seq_len", rewrite(d$dimnames$length))
    extract <- call("[", state, call("+", rewrite(x$offset), seq))
    if (d$rank > 1L) {
      extract <- call("array", extract, generate_r_dim(d, rewrite))
    }
  }
  extract
}


r_expr_block <- function(exprs) {
  if (is.language(exprs)) {
    exprs <- list(exprs)
  } else {
    exprs <- r_flatten_eqs(exprs)
  }
  as.call(c(list(as.name("{")), exprs))
}


r_expr_if <- function(condition, a, b) {
  call("if", condition, r_expr_block(a), r_expr_block(b))
}


r_expr_local <- function(exprs) {
  call("local", r_expr_block(exprs))
}
