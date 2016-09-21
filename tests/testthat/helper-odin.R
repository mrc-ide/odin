TEST_VERBOSE <- FALSE

on_appveyor <- function() {
  identical(Sys.getenv("APPVEYOR"), "True")
}

unload_dlls <- function() {
  gc() # force GC or we could get issues when gc'ing a pointer with no dll
  drop <- .dlls$get()
  err <- vlapply(drop, function(x)
    inherits(try(dyn.unload(x), silent = TRUE), "try-error"))
  environment(.dlls$add)$res <- drop[err]
}
