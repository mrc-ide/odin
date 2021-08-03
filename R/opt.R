## things not done:
## Can resolve x - y for numeric args
## Can simplify a + b - c by rewriting as a + b + (-c)
## Pointless parens
## Don't cope with unary +/-
## Factorise simple linear combinations in +?

## Part of the point of this is to assemble expressions into forms
## that an optimising compiler later in the chain can simplify.
static_eval <- function(expr) {
  if (!is.recursive(expr)) {
    return(expr)
  }

  if (is_call(expr, "+") || is_call(expr, "*")) {
    expr <- static_eval_assoc(expr)
  } else if (is_call(expr, "if")) {
    expr <- static_eval_if(expr)
  } else {
    expr[-1] <- lapply(expr[-1], static_eval)
  }

  if (is_call(expr, "(") && length(expr) == 2L) {
    expr <- expr[[2L]]
  }

  expr
}


static_eval_assoc <- function(expr) {
  expr <- flatten_assoc(expr)
  expr[-1] <- lapply(expr[-1], static_eval)

  ## We need a *second* round here of flatten_assoc
  expr <- flatten_assoc(expr)

  fn <- as.character(expr[[1]])
  args <- expr[-1L]

  i <- vlapply(args, is.numeric)
  if (any(i)) {
    args <- c(args[!i], eval(r_fold_call(fn, args[i]), baseenv()))
  }

  if (fn == "+") {
    args <- args[!vlapply(args, function(x) is.numeric(x) && x == 0)]
    if (length(args) == 0) {
      return(0)
    }

    ## Collect linear combinations of shared parameters here; this
    ## causes issues for simplifying general expressions (e.g., a + 1
    ## * (a + a) will end up as 2 * a + a) but odin doesn't generate
    ## things like that (yet).
    i <- match(args, args)
    if (anyDuplicated(i)) {
      for (k in unique(i[duplicated(i)])) {
        args[[k]] <- call("*", args[[k]], as.numeric(sum(i == k)))
      }
      args <- args[!duplicated(i)]
    }
  }

  if (fn == "*") {
    if (any(vlapply(args, function(x) is.numeric(x) && x == 0))) {
      args <- list(0)
    }
    args <- args[!vlapply(args, function(x) is.numeric(x) && x == 1)]
  }

  if (length(args) == 1L) {
    return(args[[1L]])
  }

  r_fold_call(fn, order_args(args))
}


static_eval_if <- function(expr) {
  args <- lapply(expr[-1], static_eval)

  cond <- args[[1L]]
  if (is.recursive(cond) && all(vlapply(cond[-1L], is.numeric))) {
    cond <- eval(cond, baseenv())
  }

  if (!is.recursive(cond)) {
    expr <- if (as.logical(cond)) args[[2L]] else args[[3L]]
  } else {
    expr[-1L] <- args
  }

  expr
}


order_args <- function(args) {
  i <- viapply(args, function(x) is.language(x) + is.recursive(x))
  args[order(-i, vcapply(args, deparse_str))]
}


flatten_assoc <- function(expr) {
  fn <- expr[[1L]]
  check <- as.list(expr[-1L])
  args <- list()
  while (length(check) > 0) {
    i <- vlapply(check, is_call, fn)
    args <- c(args, check[!i])
    check <- unlist(lapply(check[i], function(x) as.list(x[-1])), FALSE)
  }

  c(list(fn), args)
}
