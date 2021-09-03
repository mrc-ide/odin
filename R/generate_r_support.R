## Functions that are used *by* the generated R code; these will be
## directly added to the environment.  These functions are more like
## the library.c functions for the C generation.


## In an effort to build the minimal set of functions needed, here is
## a fully "known" set of functions.  This will grow as more functions
## are added but stops the issue of not explicitly supporting
## functions and getting drift between different implementations.
## Eventually even 'base' might want to change here.
odin_base_env <- function() {
  env <- new.env(parent = as.environment("package:base"))

  stats <- as.environment("package:stats")
  imports <- grep("_rand$", names(FUNCTIONS_STOCHASTIC),
                  invert = TRUE, value = TRUE)
  for (i in imports) {
    env[[i]] <- stats[[i]]
  }
  env[["rmhyper"]] <- rmhyper

  env
}


## Some support functions - these are not subject to code generation
## at all and will be injected into the appropriate environment.
support_get_user_double <- function(user, name, internal, size, default,
                                    min, max, integer) {
  value <- user[[name]]
  if (is.null(value)) {
    if (is.null(internal[[name]])) {
      if (is.null(default)) {
        stop(sprintf("Expected a value for '%s'", name), call. = FALSE)
      } else {
        value <- default
      }
    } else {
      ## This has the slightly annoying property of setting the value
      ## to itself but that's harmless in the face of other
      ## inefficiencies and preserves this as a pure function.
      value <- internal[[name]]
    }
  } else {
    value <- support_coerce_mode(value, integer, min, max, name)
    d <- dim(value)
    if (is.null(size)) {
      if (length(value) != 1L || !is.null(d)) {
        stop(sprintf("Expected a scalar numeric for '%s'", name), call. = FALSE)
      }
    } else if (length(size) == 1L) {
      if (length(value) != size || !is.null(d)) {
        stop(sprintf("Expected length %d value for '%s'", size, name),
             call. = FALSE)
      }
    } else {
      if (length(d) != length(size) || any(d != size)) {
        stop(sprintf("Expected a numeric array with dimensions %s for '%s'",
                     paste(size, collapse = " * "), name), call. = FALSE)
      }
    }
  }
  value
}


## This one works entirely through side effects to avoid the confusion
## and any ambiguity about what is set where.
support_get_user_dim <- function(user, internal, name, len, dims,
                                 min, max, integer) {
  value <- user[[name]]
  if (is.null(value) && !is.null(internal[[name]])) {
    ## Leave previous value alone:
    return()
  }
  if (is.null(value)) {
    stop(sprintf("Expected a value for '%s'", name), call. = FALSE)
  }
  value <- support_coerce_mode(value, integer, min, max, name)
  d <- dim(value)
  if (is.null(dims)) {
    if (!is.null(d)) {
      stop(sprintf("Expected a numeric vector for '%s'", name),
           call. = FALSE)
    }
  } else {
    rank <- length(dims)
    if (length(d) != rank) {
      stop(sprintf("Expected a numeric array of rank %d for '%s'", rank, name),
           call. = FALSE)
    }
    for (i in seq_len(rank)) {
      internal[[dims[[i]]]] <- d[[i]]
    }
  }
  internal[[len]] <- length(value)
  internal[[name]] <- value
}


support_check_interpolate_y <- function(dim_arg, dim_target, name_arg,
                                        name_target) {
  rank <- length(dim_target) - 1L
  stopifnot(length(dim_target) == length(dim_arg))
  if (rank == 0L) {
    if (dim_arg != dim_target) {
      stop(sprintf("Expected %s to have length %d (for '%s')",
                   name_arg, dim_target, name_target), call. = FALSE)
    }
  } else {
    ## TODO: this can be simplifed after tests are passing; we want to
    ## match errors at the moment.
    i <- dim_arg != dim_target
    if (any(i)) {
      j <- which(i)[[1L]]
      stop(sprintf("Expected dimension %d of %s to have size %d (for '%s')",
                   j, name_arg, dim_target[[j]], name_target),
           call. = FALSE)
    }
  }
}


support_check_interpolate_t <- function(time, dat, tcrit) {
  if (is.null(dat)) {
    return(tcrit)
  }
  if (time[[1]] < dat$min) {
    stop(sprintf("Integration times do not span interpolation range; min: %s",
                 dat$min), call. = FALSE)
  }
  if (time[[length(time)]] > dat$max) {
    stop(sprintf("Integration times do not span interpolation range; max: %s",
                 dat$max), call. = FALSE)
  }
  if (length(dat$max) > 0L && is.null(tcrit)) {
    ## > min(dat$max, tcrit) # TODO: breaks tests, but better behaviour
    dat$max
  } else {
    tcrit
  }
}


support_coerce_mode <- function(value, integer, min, max, name) {
  if (integer) {
    if (!is_integer_like(value)) {
      stop(sprintf("Expected '%s' to be integer-like", name), call. = FALSE)
    }
    storage.mode(value) <- "integer"
  } else if (is.integer(value)) {
    storage.mode(value) <- "numeric"
  } else if (!is.numeric(value)) {
    stop(sprintf("Expected a numeric value for '%s'", name), call. = FALSE)
  }
  if (any(is.na(value))) {
    stop(sprintf("'%s' must not contain any NA values", name), call. = FALSE)
  }
  if (!is.null(min) && any(value < min)) {
    stop(sprintf("Expected '%s' to be at least %s", name, min),
         call. = FALSE)
  }
  if (!is.null(max) && any(value > max)) {
    stop(sprintf("Expected '%s' to be at most %s", name, max),
         call. = FALSE)
  }

  value
}
