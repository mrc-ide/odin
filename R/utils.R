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
       get=function() res)
}
pastec <- function(..., collapse=", ") {
  paste(..., collapse=collapse)
}
