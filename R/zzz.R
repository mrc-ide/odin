.odin <- new.env(parent = emptyenv())


.onLoad <- function(libname, pkgname) {
  .odin$version <- utils::packageVersion("odin")
  .odin$model_cache_c <- R6_cache$new(getOption("odin.cache_size", 30L))
}
