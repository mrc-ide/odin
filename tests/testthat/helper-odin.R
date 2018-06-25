TEST_VERBOSE <- FALSE

on_appveyor <- function() {
  identical(Sys.getenv("APPVEYOR"), "True")
}

unload_dlls <- function() {
  model_cache_clear()
  gc()
}
