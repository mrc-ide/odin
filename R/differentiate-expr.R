## Differentiation support for (eventually) the odin DSL (and not for
## R generally). This will handle the same array and summation
## semantics as used in odin, as well as differentiation of stochastic
## processes made deterministic.

## There are two levels here;
##
## 1. differentiate, via the list of functions "derivative" does the
##    differentiation of each expression
## 2. expression writing, via the environment of functions "maths"
##    does a moderately sensible job of writing expressions with a bit
##    of simplification.
##
## The maths step can be entirely removed but you do end up with lots
## of really ugly expressions with extra parentheses and things
## multiplied by zero that should be removed.
differentiate <- function(expr, name) {
  if (is.numeric(expr)) {
    0
  } else if (is.symbol(expr)) {
    if (identical(expr, as.symbol(name))) 1 else 0
  } else {
    fn <- as.character(expr[[1]])
    if (!fn %in% names(derivative)) {
      stop(sprintf("Unsupported function '%s' in differentiate()", fn))
    }
    derivative[[fn]](expr, name)
  }
}


derivative <- list(
  `+` = function(expr, name) {
    maths$plus(differentiate(expr[[2]], name),
               differentiate(expr[[3]], name))
  },
  `-` = function(expr, name) {
    if (length(expr) == 3) {
      maths$minus(differentiate(expr[[2]], name),
                  differentiate(expr[[3]], name))
    } else {
      maths$uminus(differentiate(expr[[2]], name))
    }
  },
  `*` = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    b <- maths$rewrite(expr[[3]])
    da <- differentiate(a, name)
    db <- differentiate(b, name)
    maths$plus(maths$times(da, b), maths$times(a, db))
  },
  `/` = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    b <- maths$rewrite(expr[[3]])
    da <- differentiate(a, name)
    db <- differentiate(b, name)
    maths$minus(
      maths$divide(da, b),
      maths$divide(maths$times(a, db), maths$times(b, b)))
  },
  `^` = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    b <- maths$rewrite(expr[[3]])
    da <- differentiate(a, name)
    db <- differentiate(b, name)
    if (maths$is_zero(db)) {
      maths$times(b, maths$times(maths$pow(a, maths$minus(b, 1)), da))
    } else if (maths$is_zero(da)) {
      maths$times(maths$pow(a, b), maths$times(call("log", a), db))
    } else {
      ## a^(b - 1) * (b da + a log(a) db)
      maths$times(
        maths$pow(a, maths$minus(b, 1)),
        maths$plus(maths$times(b, da),
                   maths$times(a, maths$times(call("log", a), db))))
    }
  },
  `(` = function(expr, name) {
    differentiate(expr[[2]], name)
  },
  exp = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    maths$times(differentiate(a, name), call("exp", a))
  },
  log = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    maths$divide(differentiate(a, name), a)
  },
  sqrt = function(expr, name) {
    maths$divide(differentiate(expr[[2]], name),
                 maths$times(2, maths$rewrite(expr)))
  },
  `if` = function(expr, name) {
    condition <- maths$rewrite(expr[[2]])
    da <- differentiate(expr[[3]], name)
    db <- differentiate(expr[[4]], name)
    if (identical(da, db)) da else call("if", condition, da, db)
  },
  lfactorial = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    maths$times(differentiate(a, name), call("digamma", maths$plus(a, 1)))
  },
  abs = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    maths$times(differentiate(a, name), call("sign", a))
  }
)

maths <- local({
  .parentheses_except <- function(x, except) {
    if (is.recursive(x)) {
      fn <- as.character(x[[1]])
      if (fn == "(") {
        return(.parentheses_except(x[[2]], except))
      }
      pass <- grepl("^[a-z]", fn) ||
        (length(except) > 0 && fn %in% except) ||
        "unary_minus" %in% except && .is_unary_minus(x)
      if (pass) {
        return(x)
      }
      call("(", x)
    } else {
      x
    }
  }
  .drop_parens <- function(x) {
    if (is_call(x, "(")) .drop_parens(x[[2]]) else x
  }
  .is_unary_minus <- function(expr, recurse = FALSE) {
    (is.numeric(expr) && expr < 0) ||
      (is_call(expr, "-") && length(expr) == 2) ||
      (recurse && (
        (is_call(expr, "*") || is_call(expr, "/")) &&
        .is_unary_minus(expr[[2]], TRUE)))
  }
  is_zero <- function(x) {
    is.numeric(x) && x == 0
  }
  is_one <- function(x) {
    is.numeric(x) && x == 1
  }
  is_minus_one <- function(x) {
    is.numeric(x) && x == -1
  }
  plus <- function(a, b) {
    ## there's more cancelling here that could be done with
    ## expressions that involve subtraction and numbers, but I don't
    ## see them turning up anywhere yet; probably best to implement
    ## after we have them.
    if (is.numeric(a) && is.numeric(b)) {
      a + b
    } else if (is.numeric(b)) {
      plus(b, a)
    } else if (is_zero(a)) {
      .drop_parens(b)
    } else if (is_call(b, "+")) {
      plus(plus(a, b[[2]]), b[[3]])
    } else {
      call("+", a, b)
    }
  }
  minus <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      a - b
    } else if (is_zero(b)) {
      .drop_parens(a)
    } else if (is_zero(a)) {
      uminus(b)
    } else {
      call("-", a, .parentheses_except(b, c("*", "/", "^")))
    }
  }
  uminus <- function(a) {
    if (is.numeric(a)) {
      -a
    } else if (.is_unary_minus(a)) {
      a[[2]]
    } else if (is_call(a, "*")) {
      if (.is_unary_minus(a[[2]])) {
        times(a[[2]][[2]], a[[3]])
      } else if (.is_unary_minus(a[[3]])) {
        times(a[[2]], a[[3]][[2]])
      } else {
        times(uminus(a[[2]]), a[[3]])
      }
    } else if (is_call(a, "/")) {
      divide(uminus(a[[2]]), a[[3]])
    } else if (is_call(a, "-") && length(a) == 3) {
      minus(a[[3]], a[[2]])
    } else if (is_call(a, "(")) {
      uminus(.drop_parens(a))
    } else {
      call("-", .parentheses_except(a, c("*", "/", "^")))
    }
  }
  times <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      a * b
    } else if (is.numeric(b)) {
      ## below here, we can assume that 'b' is a language expression
      times(b, a)
    } else if (is_zero(a)) {
      0
    } else if (is_one(a)) {
      rewrite(b)
    } else if (is_minus_one(a)) {
      uminus(b)
    } else if (is_call(a, "/")) {
      ## we have (a2 / a3 * b -> a2 * b / a3)
      divide(times(a[[2]], b), a[[3]])
    } else if (is_call(b, "/")) {
      ## we have (a * (b2 / b3)) -> (a * b2) / b3
      divide(times(a, b[[2]]), b[[3]])
    } else if (is_call(b, "*") && is.numeric(b[[2]])) {
      times(times(a, b[[2]]), b[[3]])
    } else {
      if (.is_unary_minus(b, TRUE)) {
        a <- uminus(a)
        b <- uminus(b)
      }
      aa <- .parentheses_except(a, c("*", "unary_minus", "/", "^"))
      bb <- .parentheses_except(b, c("*", "^"))
      if (is_call(bb, "*")) {
        call("*", call("*", aa, bb[[2]]), bb[[3]])
      } else {
        call("*", aa, bb)
      }
    }
  }
  divide <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      a / b
    } else if (is_one(b)) {
      a
    } else if (is_zero(a)) {
      0
    } else if (is_call(a, "/")) {
      divide(a[[2]], times(a[[3]], b))
    } else if (is_call(b, "/")) {
      times(a, divide(b[[3]], b[[2]]))
    } else {
      if (.is_unary_minus(b, TRUE)) {
        a <- uminus(a)
        b <- uminus(b)
      }
      call("/",
           .parentheses_except(a, c("*", "unary_minus", "^")),
           .parentheses_except(b, "^"))
    }
  }
  pow <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      a^b
    } else if (is_one(b)) {
      a
    } else if (is_zero(b)) {
      1
    } else if (is.numeric(b) && b == 2 && is.symbol(a)) {
      times(a, a)
    } else {
      call("^", .parentheses_except(a, NULL), .parentheses_except(b, NULL))
    }
  }
  rewrite <- function(expr) {
    if (is.recursive(expr)) {
      fn <- as.character(expr[[1]])
      args <- lapply(expr[-1], rewrite)
      if (fn == "+") {
        plus(args[[1]], args[[2]])
      } else if (fn == "-" && length(expr) == 3) {
        minus(args[[1]], args[[2]])
      } else if (fn == "-" && length(expr) == 2) {
        uminus(args[[1]])
      } else if (fn == "*") {
        times(args[[1]], args[[2]])
      } else if (fn == "/") {
        divide(args[[1]], args[[2]])
      } else if (fn == "*") {
        times(args[[1]], args[[2]])
      } else if (fn == "^") {
        pow(args[[1]], args[[2]])
      } else if (fn == "(") {
        args[[1]]
      } else {
        as.call(c(list(expr[[1]]), args))
      }
    } else {
      expr
    }
  }
  as.list(environment())
})
