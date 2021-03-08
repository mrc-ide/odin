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

  fn <- expr[[1]]
  if (is_call(expr, "+") || is_call(expr, "*")) {
    expr <- static_eval_assoc(expr)
  } else {
    expr[-1] <- lapply(expr[-1], static_eval)
  }

  if (is_call(expr, "(") && length(expr) == 2L) {
    expr <- expr[[2L]]
  }

  expr
}


static_eval_assoc <- function(expr) {
  fn <- as.character(expr[[1]])
  args <- collect_assoc(lapply(expr[-1], static_eval), fn)

  i <- vlapply(args, is.numeric)
  if (any(i)) {
    args <- c(args[!i], eval(r_fold_call(fn, args[i]), baseenv()))
  }

  if (length(args) == 1L) {
    return(args[[1L]])
  }

  r_fold_call(fn, order_args(args))
}


collect_assoc <- function(args, fn) {
  args <- as.list(args)
  i <- vlapply(args, is_call, fn)
  if (any(i)) {
    args[i] <- lapply(args[i], function(x) collect_assoc(x[-1], fn))
    flatten1(args)
  } else {
    args
  }
}


order_args <- function(args) {
  i <- viapply(args, function(x) is.language(x) + is.recursive(x))
  args[order(i, decreasing = TRUE)]
}
