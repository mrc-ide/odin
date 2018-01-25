ODIN_VERSION <- NULL
.onLoad <- function(libname, pkgname) {
  ODIN_VERSION <<- odin_version() # nocov
}
