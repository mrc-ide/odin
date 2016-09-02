##' @importFrom stats na.omit setNames
##' @importFrom utils modifyList
vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
viapply <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
}
vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

## Like deparse() but always produce a single string
deparse_str <- function(x) {
  paste(deparse(x), collapse="\n")
}

collector <- function(init=character(0)) {
  res <- init
  list(add=function(x, ...) res <<- c(res, sprintf(x, ...)),
       length=function(x) length(res), # used only in debugging below
       get=function() res)
}

## We'll use this in the main loop.  This could quite happily get out
## of whack, so there are some checks here that can be removed in
## eventual production.
collector_named <- function(required=FALSE) {
  data <- collector()
  names <- collector()
  list(
    add=function(..., name="") {
      n <- max(lengths(list(...)))
      nms <- rep_len(name, n)
      if (required && !all(nzchar(nms))) {
        stop("required names are missing [odin bug]") # nocov
      }
      names$add(nms)
      n_prev <- data$length()
      data$add(...)
      if (data$length() != n_prev + n) {
        stop("odin bug") # nocov
      }
    },
    get=function() {
      setNames(data$get(), names$get())
    }
  )
}

collector_list <- function(init=list()) {
  res <- init
  list(add=function(x) res <<- c(res, list(x)),
       get=function() res)
}

pastec <- function(..., collapse=", ") {
  paste(..., collapse=collapse)
}

indent <- function(x, n=2) {
  if (length(x) == 0L && is.character(x)) {
    return(x)
  } else {
    x <- unlist(strsplit(x, "\\n", fixed=TRUE), use.names=FALSE)
    if (length(x) > 0L) {
      paste0(strrep(n), x)
    } else {
      stop("should never happen [odin bug]") # nocov
    }
  }
}

strrep <- function(n, x=" ") {
  paste(rep(x, n), collapse="")
}

is_integer_like <- function(x, tol=sqrt(.Machine$double.eps)) {
  is.integer(x) || (is.numeric(x) && abs(x - round(x)) < tol)
}

is_call <- function(expr, symbol) {
  is.recursive(expr) && identical(expr[[1L]], symbol)
}

is_directory <- function(path) {
  file.exists(path) && file.info(path, extra_cols=FALSE)$isdir
}

basename_no_ext <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

substitute_ <- function(expr, env) {
  eval(substitute(substitute(y, env), list(y = expr)))
}

expand_grid_int <- function(x) {
  if (length(x) == 1L) {
    matrix(seq_len(x), x, 1L)
  } else if (length(x) >= 2L) {
    unname(as.matrix(do.call("expand.grid", lapply(x, seq_len), quote=TRUE)))
  }
}

is_duplicated <- function(x) {
  x %in% x[duplicated(x)]
}

names_if <- function(x) {
  names(x)[x]
}

rbind_as_df <- function(x) {
  do.call("rbind",
          lapply(x, as.data.frame, stringsAsFactors=FALSE),
          quote=TRUE)
}

self <- NULL
