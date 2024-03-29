make_names <- function(variable_order, output_order, discrete) {
  ord <- c(variable_order, output_order)
  if (all(vlapply(ord, is.null))) {
    nms <- names(ord)
  } else {
    f <- function(x) {
      if (is.null(x)) {
        ""
      } else {
        sprintf("[%s]", apply(expand_grid_int(x), 1, paste, collapse = ","))
      }
    }
    idx <- unname(lapply(ord, f))
    nms <- sprintf("%s%s", rep(names(ord), lengths(idx)), unlist(idx))
  }
  c(if (discrete) STEP else TIME, nms)
}


support_n_out <- function(output_order) {
  n <- vnapply(output_order, function(x) if (is.null(x)) 1L else prod(x))
  as.integer(sum(n))
}


## TODO: this should return a generator
support_transform_variables <- function(y, private) {
  ord <- c(setNames(list(NULL), TIME),
           private$variable_order,
           private$output_order)
  j <- seq_along(private$variable_order)
  n <- length(ord)
  len <- vnapply(ord, function(x) if (is.null(x)) 1L else prod(x),
                 USE.NAMES = FALSE)
  i1 <- cumsum(len)
  i0 <- c(1L, i1[-n] + 1L)
  tot <- sum(len)
  is_scalar <- vlapply(ord, is.null)
  is_array <- !is_scalar
  nms <- names(ord)

  ## Below here can be put into a closure of 'y'
  ny <- if (is.array(y)) ncol(y) else length(y)

  has_time <- ny == tot
  if (!has_time) {
    if (ny != tot - 1L) {
      stop("Unexpected size input")
    }
    i0 <- i0 - 1L
    i1 <- i1 - 1L
    i0[1L] <- NA_integer_
    i1[1L] <- NA_integer_
  }

  ret <- setNames(vector("list", n), nms)

  if (is.matrix(y)) {
    ## Here, it might make sense to treat length1 arrays as scalars,
    ## but that might complicate things in the way that sapply does.
    ## Probably length1 arrays should be kept as arrays...
    if (any(is_scalar)) {
      ret[is_scalar] <- lapply(i0[is_scalar], function(i) y[, i])
    }
    if (any(is_array)) {
      nt <- nrow(y)
      ret[is_array] <- lapply(which(is_array), function(i) {
        array(y[, i0[[i]]:i1[[i]]], c(nt, ord[[i]]))
      })
    }
  } else if (is.array(y)) {
    if (any(is_scalar)) {
      ret[is_scalar] <- lapply(i0[is_scalar], function(i) {
        adrop(y[, i, , drop = FALSE], 2L)
      })
    }
    if (has_time) {
      ret[[1]] <- ret[[1]][, 1L, drop = TRUE]
    }
    if (any(is_array)) {
      nt <- nrow(y)
      nr <- dim(y)[[3L]]
      ret[is_array] <- lapply(which(is_array), function(i) {
        array(y[, i0[[i]]:i1[[i]], ], c(nt, ord[[i]], nr))
      })
    }
  } else {
    if (any(is_scalar)) {
      ret[is_scalar] <- y[i0[is_scalar]]
    }
    if (any(is_array)) {
      shape_array <- function(x, ord) {
        if (length(ord) == 1L) unname(x) else array(x, ord)
      }
      ret[is_array] <- lapply(which(is_array), function(i) {
        shape_array(y[i0[[i]]:i1[[i]]], ord[[i]])
      })
    }
  }
  ret
}


support_check_user <- function(user, allowed, unused_user_action) {
  given <- names(user)
  if (length(user) > 0 && (is.null(given) || !all(nzchar(given)))) {
    stop("All user parameters must be named", call. = FALSE)
  }
  err <- unique(given[duplicated(given)])
  if (length(err) > 0L) {
    stop("Duplicated user parameters: ", paste(err, collapse = ", "),
         call. = FALSE)
  }
  err <- setdiff(given, allowed)
  if (length(err) > 0L) {
    unused_user_action <- unused_user_action %||%
      getOption("odin.unused_user_action", "warning")

    msg <- paste("Unknown user parameters:", paste(err, collapse = ", "))
    switch(unused_user_action,
           ignore = NULL,
           message = message(msg),
           warning = warning(msg, call. = FALSE, immediate. = TRUE),
           stop = stop(msg, call. = FALSE),
           stop(paste(msg, "(and invalid value for unused_user_action)"),
                call. = FALSE))
  }
}


as_integer <- function(x, name = deparse(substitute(x))) {
  if (is.integer(x)) {
    x
  } else if (is.numeric(x)) {
    ret <- as.integer(x)
    if (max(abs(ret - x)) > sqrt(.Machine$double.eps)) {
      stop(sprintf("Expected integer input for '%s'", name), call. = FALSE)
    }
    ret
  } else {
    stop(sprintf("Expected integer input for '%s'", name), call. = FALSE)
  }
}


as_numeric <- function(x, name = deparse(substitute(x))) {
  if (is.integer(x)) {
    as.numeric(x)
  } else if (is.numeric(x)) {
    x
  } else {
    stop(sprintf("Expected numeric input for '%s'", name), call. = FALSE)
  }
}


## https://en.wikipedia.org/wiki/Hypergeometric_distribution
## (section "Multivariate hypergeometric distribution")
##
## > the model of an urn with green and red marbles can be extended to
## > the case where there are more than two colors of marbles. If
## > there are K_i marbles of color i in the urn and you take n
## > marbles at random without replacement, then the number of marbles
## > of each color in the sample (k_1, k_2, ..., k_c) has the
## > multivariate hypergeometric distribution. This has the same
## > relationship to the multinomial distribution that the
## > hypergeometric distribution has to the binomial distribution—the
## > multinomial distribution is the "with-replacement" distribution
## > and the multivariate hypergeometric is the "without-replacement"
## > distribution.
##
## Expectations:
##   E(K_i) => n * k_i / N
##   Var(K_i) => n * (N - n) / (N - 1) * k_i / N * (1 - k_i / N)
##   Cov(K_i, K_j) => - n * (N - n) / (N - 1) * k_i / N * k_j / N
##' @importFrom stats rhyper
rmhyper <- function(n_sample, k) {
  N <- sum(k)
  if (n_sample > N) {
    stop(sprintf("Requesting too many elements in rmhyper (%d from %d)",
                 n_sample, N))
  }
  m <- length(k)
  ret <- rep(0, m)
  k_other <- N - k[[1L]]
  ret[[1L]] <- rhyper(1, k[[1L]], k_other, n_sample)
  for (i in seq_len(m - 1)[-1L]) {
    k_other <- k_other - k[[i]]
    n_sample <- n_sample - ret[i - 1]
    ret[[i]] <- rhyper(1, k[[i]], k_other, n_sample)
  }
  ret[[m]] <- n_sample - ret[[m - 1]]
  ret
}
