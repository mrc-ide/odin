compile <- function(filename, verbose=TRUE, load=TRUE) {
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
  if (ok != 0L) {
    stop("Error compiling source")
  }
  if (load) {
    dyn.load(paste0(base, .Platform$dynlib.ext))
  }
  base
}
