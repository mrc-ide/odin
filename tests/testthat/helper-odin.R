TEST_VERBOSE <- FALSE

options(odin.verbose = FALSE,
        odin.validate = TRUE,
        odin.target = NULL)

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


skip_for_target <- function(target, using = NULL) {
  using <- odin_options(target = using)$target
  if (target == using) {
    testthat::skip(sprintf("Engine is %s", target))
  }
}


prepare_run_tests <- function() {
  path <- "run"
  re <- "^test-run-(.*\\.R)$"
  files <- dir(path, pattern = re)
  pat <- "%TARGET%"

  targets <- c("r", "c")
  header_fmt <- "## Automatically generated from %s/%s - do not edit!"

  for (f in files) {
    txt <- readLines(file.path(path, f))
    header <- sprintf(header_fmt, path, f)

    if (!grepl(pat, txt[[1]])) {
      stop("did not find target replacement in ", f)
    }
    for (t in targets) {
      dest <- sprintf("test-run-%s-%s", t, sub(re, "\\1", f))
      message(sprintf("Writing '%s'", dest))
      res <- c(header,
               sprintf('options(odin.target = "%s")', t),
               gsub(pat, t, txt),
               "options(odin.target = NULL)")
      writeLines(res, dest)
    }
  }
}
