interpolate_prepare <- function(verbose=interactive()) {
  if ("interpolate_test" %in% names(getLoadedDLLs())) {
    invisible(FALSE)
  } else {
    writeLines(paste0("PKG_CPPFLAGS = -I", system.file(package="odin")),
               "interpolate/Makevars")
    compile("interpolate/interpolate_test.c", preclean=TRUE, verbose=verbose)
    invisible(TRUE)
  }
}
