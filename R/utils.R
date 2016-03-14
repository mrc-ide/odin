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
       prepend=function(x, ...) res <<- c(sprintf(x, ...), res),
       clear=function() res <<- init,
       pop=function() {ret <- res; res <<- init; ret},
       get=function() res)
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
  x <- unlist(strsplit(x, "\\n", fixed=TRUE), use.names=FALSE)
  if (length(x) > 0L) {
    paste0(strrep(n), x)
  } else {
    x
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
