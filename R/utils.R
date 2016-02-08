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

source_line <- function(x) {
  line <- utils::getSrcLocation(x, "line")
  if (is.null(line)) {
    "<source references not available>"
  } else {
    sprintf("line %d", line)
  }
}

indent <- function(str, n) {
  indent <- paste(rep(" ", n), collapse="")
  paste(indent, strsplit(str, "\n", fixed=TRUE)[[1L]],
        sep="", collapse="\n")
}

deparse_str <- function(x) {
  paste(deparse(x), collapse="\n")
}
