TEST_VERBOSE <- FALSE

on_appveyor <- function() {
  identical(Sys.getenv("APPVEYOR"), "True")
}

unload_dlls <- function() {
  ## Clear the cache first to drop generators that might have any
  ## interaction with the dlls.
  model_cache_clear()
  ## force GC or we could get issues when gc'ing a pointer with no dll
  gc()
  drop <- .dlls$get()
  err <- vlapply(drop, function(x)
    inherits(try(dyn.unload(x), silent = TRUE), "try-error"))
  environment(.dlls$add)$res <- drop[err]
}


## access private environment for testing
r6_private <- function(cl) {
  environment(cl$initialize)$private
}
