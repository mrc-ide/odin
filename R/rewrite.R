## This will rewrite the core C bits:
##
## * do not do integer division by casting to double
## * swap
##    - ^ to pow
##    - %% to %
##    - %/% to (int) x / (int) y
## we really need type information to do the divisions correctly.
##
## TODO: do the checking for validity elsewhere, whcih means
## publishing that here perhaps
##
## sum
## sin &c
rewrite_c <- function(expr, name_pars,
                      lookup=character(0), index=character(0),
                      custom=character(0)) {
  tr <- c("^"="pow")
  unary <- c("+", "-")
  infix <- c("+", "/", "-", "*",
             ">", "<", ">=", "<=", "==", "!=")
  ## TODO: Need a whole set of translated functions perhaps (e.g., how
  ## sum(x) -> odin_sum(x, dim_x))
  ## TODO: %/% -> ((int) a / (int) b)
  ## TODO: %% -> a % b
  rewrite <- c("sum", "dim", "length", "if", "abs", "%%", "log", "min", "max",
               "interpolate")
  allowed <- c("(", "[", infix,
               "pow", "exp", "log2", "log10", "sqrt",
               "cos", "sin", "tan", "acos", "asin", "atan",
               "cosh", "sinh", "tanh", "acosh", "asinh", "atanh",
               rewrite, custom)
  rewrite_recall <- function(x) rewrite_c(x, name_pars, lookup, index, custom)

  ## Possibly:
  ##
  ## * gamma -> gammafn
  ## * lgamma -> lgammafn

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
      stop(sprintf("Unsupported function '%s'", nm))
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
      nd <- length(idx)
      is_numeric <- vlapply(idx, "[[", "numeric")
      values[is_numeric] <- vcapply(idx[is_numeric], function(x)
        minus1(x$value_num, rewrite_recall))
      if (!all(is_numeric)) {
        if (nd > 1 && is_index) {
          is_index <- vlapply(res[-1L], "[[", "is_index")
        }
        i <- !(is_numeric | is_index)
        ## TODO: this needs to go through minus1 but not entirely sure
        ## it's used yet.
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
      if (nd > 1L) {
        r <- function(i) {
          rewrite_recall(
            array_dim_name(as.character(expr[[2L]]), c("", "1", "12")[[i]]))
        }
        values[2:nd] <- sprintf("%s * %s", values[2:nd], vcapply(2:nd, r))
        values <- paste(values, collapse=" + ")
      }
      value <- sprintf("%s[%s]", arr, values)
    } else if (nm == "sum") {
      nd <- (length(expr) - 1L) / 3L
      ii <- seq_len(nd * 2L) + 1L
      values[ii] <- vcapply(as.list(expr[ii + 1L]), minus1, rewrite_recall)
      value <- sprintf("odin_sum%d(%s)", nd, paste(values, collapse=", "))
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
      value <- f(array_dim_name(as.character(expr[[2L]])))$value
    } else if (nm == "dim") {
      tmp <- sprintf("%s_%d", array_dim_name(as.character(expr[[2L]])),
                     expr[[3L]])
      value <- f(tmp)$value
    } else if (nm == "if") {
      ## NOTE: The ternary operator has very low precendence, so I'm
      ## going to agressively parenthesise it.  This is strictly not
      ## needed when this expression is the only element of `expr` but
      ## that's hard to detect so we'll tolerate a few additional
      ## parens for now.
      value <- sprintf("(%s ? %s : %s)",
                       values[[1L]], values[[2L]], values[[3L]])
    } else if (nm == "abs") {
      if (length(values) != 1L) {
        stop("invalid input to abs") # TODO: check elsewhere
      }
      value <- sprintf("fabs(%s)", values)
    } else if (nm == "%%") {
      value <- sprintf("fmod(%s, %s)", values[[1L]], values[[2L]])
    } else if (nm == "log") {
      if (length(values) == 1L) {
        value <- sprintf("log(%s)", values[[1L]])
      } else if (length(values) == 2L) {
        value <- sprintf("log(%s) / log(%s)", values[[1L]], values[[2L]])
      } else {
        stop("invalid input to log") # TODO: check elsewhere
      }
    } else if (nm %in% c("min", "max")) {
      if (length(values) < 2L) {
        stop(sprintf("Invalid input to %s; expected at least two arguments",
                     nm))
      }
      value <- generate_nary(paste0("f", nm), values)
    } else if (nm == "interpolate") {
      ## TODO: now we're at the point where we're basically good to
      ## go.  *But* the values here aren't actually the correct ones.
      ## We need to pass a struct that we'll arrange to build during
      ## model *initialisation*.  That will look essentially the same
      ## for all of these and will be a bit like:
      ##
      ## TODO: Get the rhs name into the expression.  Hacking it in is
      ## fine.
      type <- c("constant", "linear", "cubic")[[expr[[4]] + 1L]]
      browser()
      value <- sprintf("odin_interpolate_%s_eval(%s, %s)",
                       type, TIME, rhs_name)
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
  } else if (any(INDEX %in% find_symbols(expr)$variables)) {
    rewrite(expr)
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

generate_nary <- function(name, args) {
  if (length(args) == 1L) {
    sprintf("%s(%s)", name, args[[1L]])
  } else if (length(args) == 2L) {
    sprintf("%s(%s, %s)", name, args[[1L]], args[[2L]])
  } else {
    sprintf("%s(%s, %s)", name, args[[1L]], generate_nary(name, args[-1L]))
  }
}
