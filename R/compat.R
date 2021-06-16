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


##' @importFrom utils .DollarNames
##' @export
.DollarNames.odin_generator <- function(x, pattern) {
  .DollarNames(attr(x, "generator", exact = TRUE))
}

##' @export
print.odin_generator <- function(x, ...) {
  print(attr(x, "generator", exact = TRUE))
}

deprecated_constructor_call <- function(name) {
  calls <- sys.calls()
  n <- length(calls) - 1L # second to last call would be us
  if (n >= 1 && is.symbol(calls[[n]][[1]])) {
    nm <- as.character(calls[[n]][[1]])
  } else {
    nm <- name
  }
  warning(sprintf(
    "'%s(...)' is deprecated; please use '%s$new(...)' instead", nm, nm),
    call. = FALSE)
}
