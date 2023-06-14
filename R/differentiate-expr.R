maths <- local({
  .protect <- function(x, except) {
    if (is.recursive(x)) {
      fn <- as.character(x[[1]])
      if (fn == "(") {
        return(.protect(x[[2]], except))
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
    if (is_call(x, "(")) x[[2]] else x
  }
  .is_zero <- function(x) {
    is.numeric(x) && x == 0
  }
  .is_one <- function(x) {
    is.numeric(x) && x == 1
  }
  .is_minus_one <- function(x) {
    is.numeric(x) && x == -1
  }
  .is_unary_minus <- function(expr) {
    is_call(expr, "-") && length(expr) == 2
  }
  plus <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      a + b
    } else if (.is_zero(b)) {
      .drop_parens(a)
    } else if (.is_zero(a)) {
      .drop_parens(b)
    } else {
      call("+", a, b)
    }
  }
  minus <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      a - b
    } else if (.is_zero(b)) {
      .drop_parens(a)
    } else if (.is_zero(a)) {
      uminus(b)
    } else {
      call("-", a, b)
    }
  }
  uminus <- function(a) {
    if (is.numeric(a)) {
      -a
    } else if (.is_unary_minus(a)) {
      a[[2]]
    } else if (is_call(a, "*")) {
      if (.is_unary_minus(a[[2]])) {
        maths$times(a[[2]][[2]], a[[3]])
      } else if (.is_unary_minus(a[[3]])) {
        maths$times(a[[2]], a[[3]][[2]])
      } else {
        maths$times(uminus(a[[2]]), a[[3]])
      }
    } else if (is_call(a, "-") && length(a) == 3) {
      maths$minus(a[[3]], a[[2]])
    } else {
      call("-", .protect(a, c("*", "/", "^")))
    }
  }
  times <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      a * b
    } else if (.is_zero(a) || .is_zero(b)) {
      0
    } else if (.is_one(a)) {
      .drop_parens(b)
    } else if (.is_minus_one(a)) {
      maths$uminus(b)
    } else if (.is_one(b)) {
      .drop_parens(a)
    } else if (is_call(a, "/")) {
      ## we have (a2 / a3 * b -> a2 * b / a3)
      maths$divide(maths$times(a[[2]], b), a[[3]])
    } else {
      aa <- .protect(a, c("*", "unary_minus", "/"))
      bb <- .protect(b, c("*", "unary_minus"))
      if (is_call(b, "*")) {
        call("*", call("*", aa, bb[[2]]), bb[[3]])
      } else {
        call("*", aa, bb)
      }
    }
  }
  divide <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      a / b
    } else if (.is_one(b)) {
      a
    } else if (.is_zero(a)) {
      0
    } else if (.is_zero(b)) {
      Inf
    } else if (is_call(a, "/")) {
      maths$divide(a[[2]], maths$times(a[[3]], b))
    } else if (is_call(b, "/")) {
      maths$times(a, math$divide(b[[3]], b[[2]]))
    } else {
      ## browser()
      call("/", .protect(a, c("*", "^")), .protect(b, "^"))
    }
  }
  pow <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      a^b
    } else if (.is_one(b)) {
      a
    } else if (is.numeric(b) && b == 2 && is.symbol(a)) {
      times(a, a)
    } else {
      call("^", .protect(a, NULL), .protect(b, NULL))
    }
  }
  as.list(environment())
})

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
    a <- expr[[2]]
    b <- expr[[3]]
    da <- differentiate(a, name)
    db <- differentiate(b, name)
    maths$plus(maths$times(da, b), maths$times(a, db))
  },
  `/` = function(expr, name) {
    a <- expr[[2]]
    b <- expr[[3]]
    da <- differentiate(a, name)
    db <- differentiate(b, name)
    maths$divide(
      maths$minus(maths$times(da, b), maths$times(a, db)),
      maths$pow(b, 2))
  },
  `(` = function(expr, name) {
    differentiate(expr[[2]], name)
  },
  exp = function(expr, name) {
    maths$times(differentiate(expr[[2]], name), expr)
  },
  log = function(expr, name) {
    a <- expr[[2]]
    maths$divide(differentiate(a, name), a)
  },
  `if` = function(expr, name) {
    a <- differentiate(expr[[3]], name)
    b <- differentiate(expr[[4]], name)
    if (identical(a, b)) a else call("if", expr[[2]], a, b)
  },
  lfactorial = function(expr, name) {
    a <- expr[[2]]
    da <- differentiate(a, name)
    maths$times(da, call("digamma", maths$plus(a, 1)))
  }
)

differentiate <- function(expr, name) {
  if (is.symbol(expr)) {
    if (identical(expr, as.symbol(name))) 1 else 0
  } else if (is.numeric(expr)) {
    0
  } else if (is.recursive(expr)) {
    fn <- as.character(expr[[1]])
   if (fn %in% names(derivative)) {
      derivative[[fn]](expr, name)
    } else {
      browser()
    }
  } else {
    stop("unreachable?")
  }
}
