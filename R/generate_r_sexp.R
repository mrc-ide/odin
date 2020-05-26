generate_r_sexp <- function(x, data, meta) {
  if (is.recursive(x)) {
    fn <- x[[1L]]
    args <- x[-1L]
    if (fn == "length") {
      generate_r_sexp(data$elements[[args[[1L]]]]$dimnames$length,
                      data, meta)
    } else if (fn == "dim") {
      nm <- data$elements[[args[[1L]]]]$dimnames$dim[[args[[2L]]]]
      generate_r_sexp(nm, data, meta)
    } else if (fn == "odin_sum") {
      generate_r_sexp_sum(lapply(args, generate_r_sexp,
                                 data, meta))
    } else if (fn == "norm_rand") {
      quote(rnorm(1L))
    } else if (fn == "unif_rand") {
      quote(runif(1L))
    } else if (fn == "exp_rand") {
      quote(rexp(1L))
    } else {
      args <- lapply(args, generate_r_sexp, data, meta)
      if (fn %in% names(FUNCTIONS_STOCHASTIC) && fn != "rmhyper") {
        args <- c(list(1L), args)
      }
      if (fn == "rbinom") {
        args[[2L]] <- call("round", args[[2L]])
      }
      as.call(c(list(as.name(fn)), args))
    }
  } else if (is.character(x)) {
    location <- data$elements[[x]]$location
    if (!is.null(location) && location == "internal") {
      call("[[", as.name(meta$internal), x)
    } else {
      as.name(x)
    }
  } else if (is.integer(x)) {
    as.numeric(x)
  } else {
    x
  }
}


generate_r_sexp_sum <- function(args) {
  f <- function(a, b) {
    if (identical(a, b)) a else call("seq.int", a, b, by = 1L)
  }
  i <- seq(2L, by = 2L, to = length(args))
  idx <- Map(f, args[i], args[i + 1L])
  call("sum", as.call(c(list(as.name("["), args[[1L]]), idx)))
}
