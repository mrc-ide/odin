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
       get=function() res)
}
pastec <- function(..., collapse=", ") {
  paste(..., collapse=collapse)
}

## Drop blank lines from a string.  Used to work around some
## whisker/mustache inconsistencies.
drop_blank <- function(x) {
  sub("^\n", "", gsub("\n[[:space:]]*\n", "\n", x))
}

## Wrapper function to help with whisker
wr <- function(...) {
  res <- whisker::whisker.render(...)
  ## This is overly simple but it will do for now, given that whisker
  ## only outputs a few types:
  ##    whisker::escape --> amp, lt, gt, quot
  ## It obviously misses CDATA entities :)
  if (any(grepl("&[#a-zA-Z0-9]+;", res))) {
    stop("HTML entities detected in translated template (use triple '{'")
  }
  res
}

read_file <- function(...) {
  paste(readLines(...), collapse="\n")
}

indent <- function(x, n=2) {
  x <- unlist(strsplit(x, "\\n", fixed=TRUE), use.names=FALSE)
  if (length(x) > 0L) {
    paste0(paste(rep(" ", n), collapse=""), x)
  } else {
    x
  }
}
