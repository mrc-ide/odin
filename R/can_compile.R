##' Test if compilation appears possible.  This is used in some
##' examples, and tries compiling a trivial C program with \code{R CMD
##' SHLIB}.  Results are cached between runs within a session so this
##' should be fast to rely on.
##'
##' If this function believes you can't compile, and if \code{gcc}
##' can't be found on the path, a diagnostic message will be printed.
##' This will of course not be very interesting if you use a different
##' compiler to gcc!  But the most likely people affected here are
##' Windows users; if you get this ensure that you have rtools
##' installed.  If you have \code{devtools} installed,
##' \code{devtools::find_rtools()} may be helpful for diagnosing
##' compiler issues.
##'
##' @title Test if compilation is possible
##' @param verbose Be verbose when running commands?
##' @param refresh Try again to compile, skipping the cached value?
##' @return A logical scalar
##' @export
##' @examples
##' can_compile() # will take ~0.1s the first time
##' can_compile() # should be basically instantaneous
can_compile <- function(verbose = FALSE, refresh = FALSE) {
  ## should check if GCC is in the path?
  if (refresh || is.null(.odin$can_compile)) {
    tmp <- tempfile()
    dir.create(tmp)
    owd <- setwd(tmp)
    on.exit({
      setwd(owd)
      unlink(tmp, recursive = TRUE)
    })
    file <- "hello.c"
    writeLines("#include <R.h>", file)
    dest <- if (verbose) "" else FALSE
    Sys.setenv(R_TESTS = "")
    code <- system2(file.path(R.home(), "bin", "R"),
                    c("CMD", "SHLIB", "hello.c"),
                    stdout = dest, stderr = dest)
    if (code != 0L && Sys.which("gcc") == "") {
      message("I don't see gcc on the PATH") # nocov
    }
    .odin$can_compile <- code == 0L
  }
  .odin$can_compile %||% FALSE
}
