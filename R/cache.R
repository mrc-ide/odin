## This memoises some calls within interface.R so that we can avoid
## some expensive operations.  We could probably use a single cache
## but that's somewhat complicated by trying to avoid recompiling
## between sessions (and doing things like clobbering dlls that have
## been loaded by another process).
model_cache <- new.env(parent = emptyenv())

model_cache_clear <- function() {
  rm(list = ls(model_cache, all.names = TRUE), envir = model_cache)
}


model_cache_put <- function(hash, model, dll, skip_cache) {
  if (!skip_cache) {
    model_cache[[hash$model]] <- list(hash = hash, model = model, dll = dll)
  }
}


model_cache_get <- function(hash, skip_cache) {
  if (skip_cache) {
    return(NULL)
  }
  ret <- model_cache[[hash]]
  if (!is.null(ret$hash$includes) &&
       !isTRUE(all.equal(hash_files(names(ret$hash$includes), TRUE),
                         ret$hash$includes))) {
    ## Includes have changed so invalidate the cache:
    ret <- NULL
  }
  ret
}


model_cache_list <- function() {
  sort(ls(model_cache, all.names = TRUE))
}


hash_model <- function(x) {
  if (!isTRUE(attr(x, "odin_preprocessed"))) {
    stop("Expected preprocessed model")
  }
  ## source ref attributes are not good to keep!
  if (is.null(x$file)) {
    attributes(x$exprs) <- NULL
  }
  hash_object(list(ODIN_VERSION, x))
}
