## This memoises some calls within interface.R so that we can avoid
## some expensive operations.  We could probably use a single cache
## but that's somewhat complicated by trying to avoid recompiling
## between sessions (and doing things like clobbering dlls that have
## been loaded by another process).


## A small R6 class that allows for a very basic ring-like interface.
## I will move something like this into ring at some point
## (https://github.com/richfitz/ring/issues/11) given it's a pain to
## test and it's the sort of thing that ring should really support
## (and we depend on it already!)
R6_model_cache <- R6::R6Class(
  "model_cache",

  public = list(
    capacity = NULL,
    data = NULL,

    initialize = function() {
      self$capacity <- 30L
      self$clear()
    },

    clear = function() {
      self$data <- setNames(list(), character())
    },

    put = function(key, value) {
      keys <- names(self$data)
      new <- setNames(list(value), key)
      if (key %in% keys) {
        self$data <- c(new, self$data[setdiff(keys, key)])
      } else if (length(self$data) >= self$capacity) {
        self$data <- c(new, self$data[seq_len(self$capacity) - 1L])
      } else {
        self$data <- c(new, self$data)
      }
    },

    get = function(key) {
      self$data[[key]]
    },

    list = function() {
      names(self$data)
    },

    resize = function(capacity) {
      if (capacity > length(self$data)) {
        self$data <- self$data[seq_len(capacity)]
      }
      self$capacity <- capacity
    }
  ))


model_cache <- R6_model_cache$new()

model_cache_clear <- function() {
  model_cache$clear()
}


model_cache_put <- function(hash, model, dll, skip_cache) {
  ## There's a couple of models of how to do this here: the nicest
  ## conceptually is to use a ring buffer and just push models onto
  ## it.  If we do that then we can expire models in order.  In an
  ## ideal situation if we have a cache hit we'd _move_ the element up
  ## the ring buffer.  That's not supported in ring yet, and I'm not
  ## really sure that it should be actually.
  ##
  ## The other thing that the ring buffer does not suppore is real
  ## key/value storage with simple lookup of hashes
  if (!skip_cache) {
    model_cache$put(hash$model, list(hash = hash, model = model, dll = dll))
  }
}


model_cache_get <- function(hash, skip_cache) {
  if (skip_cache) {
    return(NULL)
  }
  ret <- model_cache$get(hash)
  if (!is.null(ret$hash$includes) &&
       !isTRUE(all.equal(hash_files(names(ret$hash$includes), TRUE),
                         ret$hash$includes))) {
    ## Includes have changed so invalidate the cache:
    ret <- NULL
  }
  ret
}


model_cache_list <- function() {
  sort(model_cache$list())
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
