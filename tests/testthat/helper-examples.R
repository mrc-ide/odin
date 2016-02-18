source1 <- function(filename) {
  x <- source(filename, local=TRUE)
  if (!is.function(x$value)) {
    stop("Did not get expected output from source")
  }
  x$value()
}

seq_range <- function(t, length.out) {
  seq(t[[1L]], t[[2L]], length.out=length.out)
}

compile <- function(filename, verbose=FALSE) {
  if (isTRUE(verbose)) {
    verbose <- ""
  }
  Sys.setenv(R_TESTS="")
  owd <- setwd(dirname(filename))
  on.exit(setwd(owd))
  base <- sub("\\.c$", "", basename(filename))
  ok <- system2(file.path(R.home(), "bin", "R"),
                c("CMD", "SHLIB", basename(filename)),
                stdout=verbose, stderr=verbose)
  if (ok == 0L) {
    dyn.load(paste0(base, .Platform$dynlib.ext))
    base
  } else {
    stop("Error compiling source")
  }
}
