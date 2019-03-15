## This memoises some calls within interface.R so that we can avoid
## some expensive operations.  We could probably use a single cache
## but that's somewhat complicated by trying to avoid recompiling
## between sessions (and doing things like clobbering dlls that have
## been loaded by another process).
##
## A small R6 class that allows for a very basic ring-like interface.
## I might move something like this into ring at some point
## (https://github.com/richfitz/ring/issues/11) given it's a pain to
## test and it's the sort of thing that ring should really support
## (and we depend on it already!)
##
## The other way of doing this would be to order the access times
R6_ring_cache <- R6::R6Class(
  "ring_cache",

  public = list(
    capacity = NULL,
    data = NULL,

    initialize = function(capacity) {
      self$capacity <- capacity
      self$clear()
    },

    clear = function() {
      self$data <- set_names(list(), character())
    },

    put = function(key, value) {
      new <- set_names(list(value), key)
      if (key %in% names(self$data)) {
        self$promote(key)
      } else if (length(self$data) >= self$capacity) {
        self$data <- c(new, self$data[seq_len(self$capacity) - 1L])
      } else {
        self$data <- c(new, self$data)
      }
    },

    get = function(key) {
      ret <- self$data[[key]]
      if (!is.null(ret)) {
        self$promote(key)
      }
      ret
    },

    promote = function(key) {
      self$data <- self$data[c(key, setdiff(names(self$data), key))]
    },

    list = function() {
      names(self$data)
    },

    resize = function(capacity) {
      if (capacity < length(self$data)) {
        self$data <- self$data[seq_len(capacity)]
      }
      self$capacity <- capacity
    }
  ))
