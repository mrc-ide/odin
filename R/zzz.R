.odin <- new.env(parent = emptyenv())


.onLoad <- function(libname, pkgname) {
  .odin$version <- utils::packageVersion("odin") # nocov
}
