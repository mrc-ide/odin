.odin <- new.env(parent = emptyenv())


odin_onload <- function() {
  .odin$version <- utils::packageVersion("odin") # nocov
  .odin$model_cache_c <- R6_cache$new(getOption("odin.cache_size", 30L))
}


.onLoad <- function(libname, pkgname) {
  odin_onload()
}
