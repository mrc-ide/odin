## This will rewrite the core C bits:
rewrite_c <- function(expr, name_pars, lookup = character(0), safe = FALSE,
                      expr_data = NULL) {
  rewrite_recall <- function(x) {
    rewrite_c(x, name_pars, lookup, safe, expr_data)
  }

  ## * pi (#define pi M_PI) or translate to M_PI
  rewrite_expr <- function(expr) {
    if (!is.recursive(expr)) {
      num <- is.numeric(expr)
      if (num) {
        str <- deparse(expr, control = "digits17") # or hexNumeric
        is_index <- FALSE
      } else {
        ## TODO: this is going to have some serious issues if we ever
        ## want to handle true character elements as they're not going
        ## to get quoted here.
        str <- as.character(expr)
        if (as.character(expr) %in% lookup) {
          str <- sprintf("%s->%s", name_pars, str)
        }
        is_index <- str %in% INDEX
      }
      return(list(numeric = is.numeric(expr),
                  value = str,
                  value_num = if (num) expr else NULL,
                  value_sym = if (num) NULL else expr,
                  is_index = is_index))
    }
    nm <- deparse(expr[[1L]])
    if (nm %in% names(FUNCTIONS_RENAME)) {
      nm <- FUNCTIONS_RENAME[[nm]]
    } else if (nm %in% FUNCTIONS_REWRITE_RF) {
      nm <- paste0("Rf_", nm)
    }

    res <- lapply(as.list(expr[-1L]), rewrite_expr)
    n <- length(res)
    values <- vcapply(res, "[[", "value")
    is_index <- nm != "[" && any(vlapply(res, "[[", "is_index"))

    if (nm == "(") {
      value <- sprintf("(%s)", values)
    } else if (nm == "[" || nm == "[<-") {
      value <- rewrite_array(expr, res, values, rewrite_recall, safe, expr_data)
    } else if (nm == "sum") {
      ## Or:
      ##
      ##   rewrite_recall(array_dim_name(deparse(expr[[2L]])))
      ##
      ## and - 1 in the format string
      len <- minus1(as.name(array_dim_name(deparse(expr[[2L]]))),
                    rewrite_recall)
      value <- sprintf("odin_sum1(%s, 0, %s)", values[[1L]], len)
    } else if (nm %in% FUNCTIONS_SUM) {
      nd <- (length(expr) - 2L) / 2L
      values[-1L] <- vcapply(as.list(expr[-(1:2)]), minus1, rewrite_recall)
      if (nd > 1L) {
        ## TODO: This appears all over the show, and is harder than ideal
        arr <- as.character(expr[[2L]])
        values <- c(values, vcapply(seq_len(nd - 1), function(x)
          rewrite_recall(array_dim_name(arr, paste(seq_len(x), collapse = "")))))
      }
      value <- sprintf("%s(%s)", nm, paste(values, collapse = ", "))
    } else if (n == 1L && nm %in% FUNCTIONS_UNARY) {
      value <- sprintf("%s%s", nm, values)
    } else if (n == 2L && nm %in% FUNCTIONS_INFIX) {
      if (nm == "/") {
        ## Special snowflake treatment for division to avoid integer
        ## division.
        ##
        ## TODO: This does not deal correctly with the expression:
        ##   (1 + 2) / (3 + 4)
        ## which would be integer division still.  So we'd be looking
        ## for any of the constituent bits to be non-integer, or a
        ## symbol that is not an integer.  Or I could just do it *always*.
        is_numeric <- vlapply(res, "[[", "numeric")
        i <- is_numeric & !grepl(".", values, fixed = TRUE)
        if (any(i)) {
          values[i] <- sprintf("(double) %s", values[i])
        }
      }
      value <- sprintf("%s %s %s", values[[1L]], nm, values[[2L]])
    } else if (nm == "length") {
      value <- rewrite_expr(array_dim_name(as.character(expr[[2L]])))$value
    } else if (nm == "dim") {
      tmp <- sprintf("%s_%d", array_dim_name(as.character(expr[[2L]])),
                     expr[[3L]])
      value <- rewrite_expr(tmp)$value
    } else if (nm == "if") {
      ## NOTE: The ternary operator has very low precendence, so I'm
      ## going to agressively parenthesise it.  This is strictly not
      ## needed when this expression is the only element of `expr` but
      ## that's hard to detect so we'll tolerate a few additional
      ## parens for now.
      value <- sprintf("(%s ? %s : %s)",
                       values[[1L]], values[[2L]], values[[3L]])
    } else if (nm == "log" && length(values) == 2L) {
      ## This is sort of an odd one to support, but it does seem
      ## potentially useful.
      value <- sprintf("log(%s) / log(%s)", values[[1L]], values[[2L]])
    } else if (nm %in% FUNCTIONS_NARY) {
      value <- generate_nary(nm, values)
    } else {
      ## This is a little extreme but is useful in at least some cases
      ## (and I don't imagine that returning NaN will be useful most
      ## of the time).
      if (nm == "Rf_rbinom") {
        values[[1]] <- sprintf("round(%s)", values[[1]])
      }
      value <- sprintf("%s(%s)", nm, paste(values, collapse = ", "))
    }

    list(numeric = FALSE, value = value, is_index = is_index)
  }

  ## I could make this optional later, but that might just be more
  ## complex than needed.
  res <- rewrite_expr(expr)
  ret <- res$value
  attr(ret, "is_index") <- res$is_index
  ret
}

rewrite_array <- function(expr, res, values, rewrite, safe, expr_data) {
  ## NOTE: This skips all entries involving index variables (i, j,
  ## k).  This is because those will be offset appropriately for
  ## us on entry because they are part of a loop.
  idx <- res[-1L]
  values <- values[-1L]
  nd <- length(idx)
  is_numeric <- vlapply(idx, "[[", "numeric")
  is_index <- vlapply(idx, "[[", "is_index")
  fix_numeric <- !is_index &  is_numeric
  fix_index   <- !is_index & !is_numeric
  ## NOTE: Cases that are is_index are already dealt with.
  values[fix_numeric] <- vcapply(which(fix_numeric), function(i)
    minus1(idx[[i]]$value_num, rewrite, i > 1L))
  values[fix_index] <- vcapply(which(fix_index), function(i)
            minus1(expr[[i + 2L]], rewrite, i > 1L))

  ## TODO: check for no unary arithmetic while indexing in main
  ## array checking (that's going to require a little work, but
  ## perhaps add it to the dependency checking functions).
  if (nd == 1L) {
    index <- values
  } else if (nd > 1L) {
    r <- function(i) {
      rewrite(array_dim_name(as.character(expr[[2L]]),
                             paste(seq_len(i - 1), collapse = "")))
    }
    index <- values
    index[2:nd] <- sprintf("%s * %s", index[2:nd], vcapply(2:nd, r))
    index <- paste(index, collapse = " + ")
  }

  target <- res[[1L]]$value

  if (safe) {
    target_str <- as.character(res[[1L]]$value_sym)
    if (nd == 1L) {
      dim <- rewrite(array_dim_name(target_str))
    } else {
      ## This is going to be all the individual indices then all the
      ## dimensions
      dim <- vcapply(seq_len(nd), function(i)
        rewrite(array_dim_name(target_str, i)))
      dim <- paste(c(values, dim), collapse = ", ")
    }

    expr_str <-
      escape_printf(odin_info_expr(expr_data$line, expr_data$expr_str))
    ## Getting newlines to print here but not break the program is a
    ## bit of a trick.  Using "\\\n" manages to break the line in the
    ## C code but not in the message!
    expr_str <- paste0("\t", expr_str, collapse = " & ")

    if (expr[[1]] == quote(`[<-`)) {
      ## This is a bit tricky: we can't really do %%s because if there
      ## are any other string format expressions in any of this it'll
      ## cause havoc.  But practically I think it's actually ok.
      fmt <- "odin_array_at_set%d(%s, %s, %%s, %s, %s, %s)"
    } else {
      fmt <- "odin_array_at%d(%s, %s, %s, %s, %s)"
    }
    ## TODO: This does not do well with initial() and I think we could
    ## preserve that name better elsewhere rather than rebuilding it
    ## here:
    ## browser()
    nm <- as.character(res[[1L]]$value_sym)
    re <- sprintf("^(%s)_(.*)", paste(SPECIAL_LHS, collapse = "|"))
    if (grepl(re, nm)) {
      nm <- sub(re, "\\1(\\2)", nm)
    }
    sprintf(fmt, nd, target, index, dim, dquote(nm), dquote(expr_str))
  } else {
    sprintf("%s[%s]", target, index)
  }
}

## The rewrite function here must be a parameterised version of the
## rewrite function above (i.e. with the last 3 elements captured by a
## closure).
minus1 <- function(expr, rewrite, paren = FALSE) {
  if (is.numeric(expr)) {
    sprintf("%d", expr - 1L)
  } else if (is.character(expr)) {
    ## sprintf("%s - 1", expr)
    stop("odin bug") # nocov
  } else {
    ret <- rewrite(expr)
    if (!isTRUE(attr(ret, "is_index"))) {
      ## TODO: this would be better if we recursed through the whole
      ## function as there are probably further simplifications that
      ## can be made to the expressions.  The compiler should be able
      ## to more than that though so it's really not that big a deal.
      if (is_call(expr, quote(`-`)) || is_call(expr, quote(`+`))) {
        if (is.numeric(expr[[2L]]) && is.numeric(expr[[3L]])) {
          expr <- eval(expr, .GlobalEnv) - 1L
        } else if (is.numeric(expr[[2L]])) {
          ret <- expr[[2L]] <- expr[[2L]] - 1L
        } else if (is.numeric(expr[[3L]])) {
          expr[[3L]] <- expr[[3L]] + if (is_call(expr, quote(`+`))) -1L else 1L
        } else {
          expr <- call("-", expr, 1)
        }
        ret <- rewrite(expr)
      } else {
        ret <- sprintf("%s - 1", ret)
      }
    }
    if (paren) {
      ret <- sprintf("(%s)", ret)
    }
    attr(ret, "is_index") <- NULL
    ret
  }
}

generate_nary <- function(name, args) {
  if (length(args) == 1L) {
    ## NOTE: If nary functions that support one arg arg implemented,
    ## this could work:
    ##
    ##     sprintf("%s(%s)", name, args[[1L]])
    stop("Not supported") # nocov
  } else if (length(args) == 2L) {
    sprintf("%s(%s, %s)", name, args[[1L]], args[[2L]])
  } else {
    sprintf("%s(%s, %s)", name, args[[1L]], generate_nary(name, args[-1L]))
  }
}
