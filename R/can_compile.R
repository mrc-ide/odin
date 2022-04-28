##' Test if compilation appears possible.  This is used in some
##' examples, and tries compiling a trivial C program with
##' `pkgbuild`.  Results are cached between runs within a session
##' so this should be fast to rely on.
##'
##' We use `pkgbuild` in order to build packages, and it includes a
##' set of heuristics to locate and organise your C compiler. The most
##' likely people affected here are Windows users; if you get this
##' ensure that you have rtools installed.  Using
##' [pkgbuild::find_rtools()] with `debug = TRUE` may be helpful for
##' diagnosing compiler issues.
##'
##' @title Test if compilation is possible
##'
##' @param verbose Be verbose when running commands?
##'
##' @param refresh Try again to compile, skipping the cached value?
##'
##' @return A logical scalar
##'
##' @export
##' @examples
##' can_compile() # will take ~0.1s the first time
##' can_compile() # should be basically instantaneous
can_compile <- function(verbose = FALSE, refresh = FALSE) {
  if (refresh || is.null(.odin$can_compile)) {
    .odin$can_compile <-
      requireNamespace("pkgbuild", quietly = verbose) &&
      tryCatch(pkgbuild::check_build_tools(verbose, !verbose),
               error = function(e) FALSE)
  }
  .odin$can_compile %||% FALSE
}
