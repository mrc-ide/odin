## This file exists to support the workaround to fix issue #206. Once
## the deprecation period is over, it can be removed, and the
## generated code changed slightly.

##' @export
`[[.odin_generator` <- function(x, i) {
  attr(x, "generator", exact = TRUE)[[i]]
}


##' @export
`$.odin_generator` <- function(x, name) {
  attr(x, "generator", exact = TRUE)[[name]]
}


##' @export
.DollarNames.odin_generator <- function(x, pattern) {
  ls(attr(x, "generator", exact = TRUE))
}


deprecated_constructor_call <- function(name) {
  calls <- sys.calls()
  if (length(calls) >= 2 && is.symbol(calls[[length(calls) - 1L]][[1]])) {
    nm <- as.character(calls[[length(calls) - 1L]][[1]])
  } else {
    nm <- name
  }
  warning(sprintf(
    "'%s(...)' is deprecated; please use '%s$new(...)' instead", nm, nm),
    call. = FALSE)
}
