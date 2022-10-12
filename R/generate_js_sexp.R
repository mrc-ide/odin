generate_js_sexp <- function(x, data, meta) {
  if (is.recursive(x)) {
    fn <- x[[1L]]
    args <- x[-1L]
    n <- length(args)
    values <- vcapply(args, generate_js_sexp, data, meta)

    if (fn == "(") {
      ret <- sprintf("(%s)", values[[1]])
    } else if (fn == "[") {
      pos <- js_array_access(args[[1L]], args[-1], data, meta)
      ret <- sprintf("%s[%s]", values[[1L]], pos)
    } else if (fn == "^") {
      ret <- sprintf("Math.pow(%s, %s)", values[[1]], values[[2]])
    } else if (n == 2L && fn %in% FUNCTIONS_INFIX) {
      ret <- sprintf("%s %s %s", values[[1]], fn, values[[2]])
    } else if (n == 1L && fn == "-") {
      ret <- sprintf("- %s", values[[1]])
    } else if (fn == "if") {
      ## NOTE: The ternary operator has very low precendence, so I'm
      ## going to agressively parenthesise it.  This is strictly not
      ## needed when this expression is the only element of `expr` but
      ## that's hard to detect so we'll tolerate a few additional
      ## parens for now.
      ret <- sprintf("(%s ? %s : %s)",
                     values[[1L]], values[[2L]], values[[3L]])
    } else if (fn == "length") {
      ret <- generate_js_sexp(data$elements[[args[[1L]]]]$dimnames$length,
                              data, meta)
    } else if (fn == "dim") {
      args[[1]] <- sub(sprintf("^%s\\.", meta$internal), "", args[[1]])
      dim <- data$elements[[args[[1L]]]]$dimnames$dim[[args[[2]]]]
      ret <- generate_js_sexp(dim, data, meta)
    } else if (fn == "log" && length(values) == 2L) {
      ret <- sprintf("(Math.log(%s) / Math.log(%s))",
                     values[[1L]], values[[2L]])
    } else if (fn == "min" || fn == "max") {
      ret <- js_fold_call(paste0("Math.", fn), values)
    } else if (fn == "sum" || fn == "odin_sum") {
      ret <- generate_js_sexp_sum(args, data, meta)
    } else if (any(names(FUNCTIONS_JS_STOCHASTIC) == fn)) {
      if (fn == "rbinom") {
        ## See equivalent logic in the C version
        ##
        ## TODO: should be this.base.maths.round2 but we don't yet
        ## import appropriate support from odin yet...
        values[[1L]] <- sprintf("Math.round(%s)", values[[1L]])
      }
      ret <- sprintf("random.%s(%s)",
                     FUNCTIONS_JS_STOCHASTIC[[fn]],
                     paste(values, collapse = ", "))
    } else {
      if (any(names(FUNCTIONS_JS_RENAME) == fn)) {
        fn <- FUNCTIONS_JS_RENAME[[fn]]
      } else if (any(FUNCTIONS_JS_MATH == fn)) {
        fn <- sprintf("Math.%s", fn)
      } else if (any(names(FUNCTIONS_JS_STOCHASTIC_SPECIAL) == fn)) {
        fn <- sprintf("random.%s", FUNCTIONS_JS_STOCHASTIC_SPECIAL[[fn]])
      } else {
        stop(sprintf("unsupported function '%s'", fn))
      }
      ret <- sprintf("%s(%s)", fn, paste(values, collapse = ", "))
    }
    ret
  } else if (is.character(x)) {
    location <- data$elements[[x]]$location
    if (!is.null(location) && location == "internal") {
      sprintf("%s.%s", meta$internal, x)
    } else {
      x
    }
  } else if (is.numeric(x)) {
    deparse(x, control = "digits17")
  }
}


## This just works the same way that the C version does, even if there
## might be a better way in js.
generate_js_sexp_sum <- function(args, data, meta) {
  target <- generate_js_sexp(args[[1]], data, meta)
  ## See https://github.com/mrc-ide/odin/pull/197
  nm <- sub(sprintf("^%s\\.", meta$internal), "", args[[1]])
  data_info <- data$elements[[nm]]

  if (length(args) == 1L) {
    len <- generate_js_sexp(data_info$dimnames$length, data, meta)
    sprintf("this.base.maths.odinSum1(%s, 0, %s)", target, len)
  } else {
    i <- seq(2, length(args), by = 2)

    all_args <- c(args, as.list(data_info$dimnames$mult[-1]))
    values <- character(length(all_args))
    values[i] <- vcapply(all_args[i], js_minus_1, FALSE, data, meta)
    values[-i] <- vcapply(all_args[-i], generate_js_sexp, data, meta)
    arg_str <- paste(values, collapse = ", ")

    sprintf("this.base.maths.odinSum%d(%s)", length(i), arg_str)
  }
}


FUNCTIONS_JS_RENAME <- c(
  "^" = "Math.pow",
  ceiling = "Math.ceil",
  round = "this.base.maths.round2",
  "%%" = "this.base.maths.modr",
  "%/%" = "this.base.maths.intdivr",
  "as.integer" = "Math.floor"
)


FUNCTIONS_JS_MATH <- c(
  "sqrt",
  "exp", "expm1", "log", "log2", "log10", "log1p",
  "cos", "sin", "tan",
  "acos", "asin", "atan", "atan2",
  "cosh", "sinh", "tanh",
  "acosh", "asinh", "atanh",
  "abs", "floor", "trunc")


FUNCTIONS_JS_STOCHASTIC_SPECIAL <- c(
  unif_rand = "randomUniform",
  norm_rand = "randomNormal",
  exp_rand = "randomExponential")


FUNCTIONS_JS_STOCHASTIC <- c(
  ## TODO: I should write out these ones somewhere
  ## And support many different distributions
  ## rbeta = "", # a, b
  rbinom = "binomial", # n, p
  ## rcauchy = "", # location, scale
  ## rchisq = "", # df
  rexp = "exponential", # rate
  ## rf = "", # n1, n2
  ## rgamma = 2L, # shape, scale
  rgeom = "geometric", # p
  ## rhyper = "", # NR, NB, n
  ## rlogis = "", # location, scale
  ## rlnorm = "logNormal", # logmean, logsd - ignoring as hard to get right
  ## rnbinom = "", # size, prob
  rnorm = "normal", # mu, sigma
  rpois = "poisson", # lambda
  ## rt = "", # n
  runif = "uniform" # a, b
  ## rweibull = "", # shape, scale
  ## rwilcox = "", # m, n
  ## rmultinom = "", # n, p
  ## rsignrank = "", # n
)
