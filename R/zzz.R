.odin <- new.env(parent = emptyenv())
ODIN_VERSION <- NULL


.onLoad <- function(libname, pkgname) {
  ODIN_VERSION <<- odin_version() # nocov
  .odin$version <- utils::packageVersion("odin")
}
