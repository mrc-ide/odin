compile <- function(filename, verbose=TRUE, load=TRUE, preclean=FALSE,
                    check_loaded=TRUE) {
  ## The actual compilation step should be very quick, so it's going
  ## to be OK to record the entire stream of output.
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
    ## Consider not doing this warning if dirname is tmpdir; though
    ## that requires normalizePath I think.
    ##
    ## NOTE: it might be good to hide this behind verbose, but it's
    ## hard to test that it works if we do that.  Instead, we could
    ## make the verbose options more granular, which might happen once
    ## I start going through and looking for errors in the gcc output;
    ##
    ##   http://stackoverflow.com/a/14923025
    message(sprintf("shared library %s%s already loaded; using %s%s",
                    orig, ext, base, ext))
  }

  dll <- paste0(base, ext)
  args <- c("CMD", "SHLIB", basename(filename),
            "-o", dll, if (preclean) "--preclean")

  output <- suppressWarnings(system2(file.path(R.home(), "bin", "R"), args,
                                     stdout=TRUE, stderr=TRUE))

  ## TODO: classify the output here, and provide information about
  ## warnings in general.  This could be done with crayon for nicer
  ## colours on a terminal too.  It'd need some serious work to get
  ## things working with both clang and gcc too, and no idea what we'd
  ## get going on other systems.  It would actually be something nice
  ## to get working across other packages...
  ok <- attr(output, "status")
  error <- !is.null(ok) && ok != 0L
  if (error || verbose) {
    cat(paste(output, "\n"), sep="")
  }
  if (error) {
    stop("Error compiling source") # nocov
  }
  if (load) {
    dyn.load(dll)
  }
  base
}
