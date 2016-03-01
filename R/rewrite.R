## This will rewrite the core C bits:
##
## * do not do integer division by casting to double
## * swap
##    - ^ to pow
##    - %% to %
##    - %/% to (int) x / (int) y
## we really need type information to do the divisions correctly.
##
## TODO: log(x, base) needs translating to a general log call
##
## TODO: do the checking for validity elsewhere, whcih means
## publishing that here perhaps
##
## sum
## sin &c
rewrite_c <- function(expr, name_pars,
                      lookup=character(0), index=character(0)) {
  tr <- c("^"="pow")
  unary <- c("+", "-")
  infix <- c("+", "/", "-", "*")
  ## TODO: Need a whole set of translated functions perhaps (e.g., how
  ## sum(x) -> odin_sum(x, dim_x))
  rewrite <- c("sum", "dim", "length")
  allowed <- c("(", "[", infix, "pow", "exp", "log", "log2", "log10", rewrite)

  ## Things that will work in R and C the same way:
  ##
  ## * cos, sin, tan, acos, asin, atan, cosh, sinh, tanh, acosh, asinh, atanh
  ## * exp, log10, log [single arg version]
  ## * sqrt
  ##
  ## Things that need a little translation
  ##
  ## * gamma -> gammafn
  ## * lgamma -> lgammafn
  ##
  ## But rather than do this, drive this from functions available in R.

  ## Things that need definitions
  ## * mod(x, y)
  ## * pi (#define pi M_PI) or translate to M_PI

  f <- function(expr) {
    if (!is.recursive(expr)) {
      num <- is.numeric(expr)
      if (num) {
        str <- deparse(expr, control="digits17") # or hexNumeric
        is_index <- FALSE
      } else {
        ## TODO: this is going to have some serious issues if we ever
        ## want to handle true character elements as they're not going
        ## to get quoted here.
        str <- as.character(expr)
        if (as.character(expr) %in% lookup) {
          str <- sprintf("%s->%s", name_pars, str)
        }
        is_index <- str %in% index
      }
      return(list(numeric=is.numeric(expr),
                  value=str,
                  value_num=if (num) expr else NULL,
                  is_index=is_index))
    }
    nm <- deparse(expr[[1L]])
    if (nm %in% names(tr)) {
      nm <- tr[[nm]]
    }
    if (!(nm %in% allowed)) {
      stop(sprintf("Unsupported function %s", nm))
    }

    res <- lapply(as.list(expr[-1L]), f)
    n <- length(res)
    values <- vcapply(res, "[[", "value")
    is_index <- any(vlapply(res, "[[", "is_index"))

    if (nm == "(") {
      stopifnot(n == 1L)
      value <- sprintf("(%s)", values)
    } else if (nm == "[") {
      ## NOTE: This skips all entries involving index variables (i, j,
      ## k).  This is because those will be offset appropriately for
      ## us on entry because they are part of a loop.
      arr <- res[[1L]]$value
      idx <- res[-1L]
      values <- values[-1L]
      n <- length(idx)
      is_numeric <- vlapply(idx, "[[", "numeric")
      ## TODO: This goes through minus1 now, though that is difficult
      ## because we really want to push the whole expression through
      ## there but I don't have access to that expression without
      ## reparsing.  Most cases though this will be a pretty simple
      ## expression so might work.
      values[is_numeric] <- vcapply(idx[is_numeric], function(x)
        as.character(as.integer(x$value_num) - 1L))

      if (!all(is_numeric)) {
        if (n > 1 && is_index) {
          is_index <- vlapply(res[-1L], "[[", "is_index")
        }
        i <- !(is_numeric | is_index)
        fmt <- if (n == 1L) "%s - 1" else "(%s - 1)"
        values[i] <- sprintf(fmt, values[i])
      }
      is_index <- FALSE

      ## TODO: check for no unary indexing in main array checking
      ## (that's going to require a little work, but perhaps add it to
      ## the dependency checking functions).

      ## We'll let
      ##   dim_1 is the number of rows (first dimension)
      ##   dim_2 is the number of columns (second dimension)
      ##   but what we really need is the product there (dim_12)
      ##   dim_3 is never used here but would be the third dimension.
      if (n > 1L) {
        values[-1L] <- sprintf("%s * dim_%s_%s", values[-1L], arr,
                               c("1", "12")[seq_len(n - 1L)])
        values <- paste(values, collapse=" + ")
      }

      value <- sprintf("%s[%s]", arr, values)
    } else if (nm == "sum") {
      ## TODO: deal with:
      ##   sum(A) # over all elements
      ##   sum(A[, 1]) # sum of first column
      ##   sum(A[i, j, 1:n]) # etc.
      if (length(expr) == 2L && is.symbol(expr[[2L]])) {
        value <- sprintf("odin_sum(%s, %s->dim_%s)",
                         res[[1L]]$value, name_pars, res[[1L]]$value)
      } else {
        stop("Unsupported version of sum")
      }
    } else if (n == 1L && nm %in% unary) {
      value <- sprintf("%s%s", nm, values)
    } else if (n == 2L && nm %in% infix) {
      if (nm == "/") {
        ## Special snowflake treatment for division to avoid integer
        ## division.
        ##
        ## TODO: This does not deal correctly with the expression:
        ##   (1 + 2) / (3 + 4)
        ## which would be integer division still.  So we'd be looking
        ## for any of the constituent bits to be non-integer, or a
        ## symbol that is not an integer.
        is_numeric <- vlapply(res, "[[", "numeric")
        i <- is_numeric & !grepl(".", values, fixed=TRUE)
        if (any(i)) {
          values[i] <- sprintf("(double) %s", values[i])
        }
      }
      value <- sprintf("%s %s %s", values[[1L]], nm, values[[2L]])
    } else if (nm == "length") {
      ## TODO: Consider replacing all other '->' bits above with
      ## recalling f().  Make that easy to do though.  Not 100% sure
      ## that's always worth doing though.
      value <- f(array_dim_name(values[[1]], TRUE))$value
    } else if (nm == "dim") {
      tmp <- sprintf("%s_%s", array_dim_name(values[[1L]], TRUE), values[[2L]])
      value <- f(tmp)$value
    } else {
      value <- sprintf("%s(%s)", nm, paste(values, collapse=", "))
    }

    list(numeric=FALSE, value=value, is_index=is_index)
  }

  f(expr)$value
}

## The rewrite function here must be a parameterised version of the
## rewrite function above (i.e. with the last 3 elements captured by a
## closure).
minus1 <- function(expr, rewrite) {
  if (is.numeric(expr)) {
    sprintf("%d", expr - 1L)
  } else if (is.character(expr)) {
    sprintf("%s - 1", expr)
  } else if (is.recursive(expr) && identical(expr[[1L]], quote(`-`))) {
    if (is.numeric(expr[[3L]])) {
      expr[[3L]] <- expr[[3L]] + 1L
    } else if (is.numeric(expr[[2L]])) {
      expr[[2L]] <- expr[[2L]] - 1L
    } else {
      expr <- call("-", expr, 1)
    }
    rewrite(expr)
  } else if (is.recursive(expr) && identical(expr[[1L]], quote(`+`))) {
    if (is.numeric(expr[[3L]])) {
      expr[[3L]] <- expr[[3L]] - 1L
    } else if (is.numeric(expr[[2L]])) {
      expr[[2L]] <- expr[[2L]] - 1L
    } else {
      expr <- call("-", expr, 1)
    }
    rewrite(expr)
  } else {
    sprintf("%s - 1", rewrite(expr))
  }
}
