compile <- function(filename, verbose=TRUE, load=TRUE, preclean=FALSE,
                    check_loaded=TRUE) {
  system_out <- if (isTRUE(verbose)) "" else verbose
  Sys.setenv(R_TESTS="")
  owd <- setwd(dirname(filename))
  on.exit(setwd(owd))
  base <- sub("\\.c$", "", basename(filename))
  ext <- .Platform$dynlib.ext

  ## This is needed because if a model is reloaded it won't work on
  ## Windows, and will trigger a weird "unlock_solver" issue with
  ## deSolve on other platforms.
  if (load && check_loaded && base %in% names(getLoadedDLLs())) {
    orig <- base
    base <- basename(tempfile(base, "."))
    if (verbose) {
      ## Consider not doing this warning if dirname is tmpdir; though
      ## that requires normalizePath I think.
      message(sprintf("shared library %s.%s already loaded; using %s.%s",
                      orig, ext, base, ext))
    }
  }

  output <- paste0(base, ext)
  args <- c("CMD", "SHLIB", basename(filename),
            "-o", output, if (preclean) "--preclean")

  ok <- system2(file.path(R.home(), "bin", "R"), args,
                stdout=system_out, stderr=system_out)
  if (ok != 0L) {
    stop("Error compiling source") # nocov
  }
  if (load) {
    dyn.load(output)
  }
  base
}
