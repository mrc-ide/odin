make_deterministic <- function(expr) {
  if (is.recursive(expr) && is.symbol(expr[[1]])) {
    fn <- as.character(expr[[1]])
    if (fn %in% names(deterministic_rules)) {
      expr <- deterministic_rules[[fn]](expr)
    }
  }
  if (is.recursive(expr)) {
    expr <- as.call(lapply(expr, make_deterministic))
  }
  expr
}


deterministic_rules <- list(
  unif_rand = function(expr) {
    0.5
  },
  norm_rand = function(expr) {
    0
  },
  exp_rand = function(expr) {
    1
  },
  rbeta = function(expr) {
    substitute(a / (a + b), list(a = expr[[2]], b = expr[[3]]))
  },
  rbinom = function(expr) {
    substitute(n * p, list(n = expr[[2]], p = expr[[3]]))
  },
  rcauchy = function(expr) {
    ## This needs to flow through to line numbers eventually, or we
    ## need to throw an error if it remains in the code (so allow it
    ## only if it is never used)
    stop("The Cauchy distribution has no mean, and may not be used")
  },
  rchisq = function(expr) {
    expr[[2]]
  },
  rexp = function(expr) {
    substitute(1 / rate, list(rate = expr[[2]]))
  },
  rf = function(expr) {
    ## TODO: only valid for df2 > 2!
    substitute(df2 / (df2 - 2), list(df2 = expr[[3]]))
  },
  rgamma = function(expr) {
    substitute(shape / rate, list(shape = expr[[2]], rate = expr[[3]]))
  },
  rgeom = function(expr) {
    substitute((1 - p) / p, list(p = expr[[2]]))
  },
  rhyper = function(expr) {
    substitute(k * m / (m + n),
               list(m = expr[[2]], n = expr[[3]], k = expr[[4]]))
  },
  rlogis = function(expr) {
    expr[[2]]
  },
  rlnorm = function(expr) {
    substitute(exp(mu + sigma^2 / 2), list(mu = expr[[2]], sigma = expr[[3]]))
  },
  rnbinom = function(expr) {
    substitute(n * (1 - p) / p, list(n = expr[[2]], p = expr[[3]]))
  },
  rnorm = function(expr) {
    expr[[2]]
  },
  rpois = function(expr) {
    expr[[2]]
  },
  rt = function(expr) {
    ## only if df > 1
    0
  },
  runif = function(expr) {
    substitute((a + b) / 2, list(a = expr[[2]], b = expr[[3]]))
  },
  rweibull = function(expr) {
    substitute(b * gamma(1 + 1 / a), list(a = expr[[2]], b = expr[[3]]))
  },
  rwilcox = function(expr) {
    substitute(m * n / 2, list(m = expr[[2]], n = expr[[3]]))
  },
  rsignrank = function(expr) {
    substitute(n * (n + 1) / 4, list(n = expr[[2]]))
  })


## These are all worked out by manually taking logarithms of the
## densities - I've not been terribly exhaustive here, but have copied
## what we use in dust already...
##
## The user is going to write out:
##
##   > compare(d) ~ poisson(lambda)
##
## which corresponds to writing
##
##   > dpois(d, lambda, log = TRUE)
##     ==> log(lambda^x * exp(-lambda) / x!)
##     ==> x * log(lambda) - lambda - lfactorial(x)
##
## All the density functions will have the same form here, with the
## lhs becoming the 'x' argument (all d* functions take 'x' as the
## first argument).
log_density <- function(distribution, target, args) {
  target <- as.name(target)
  switch(
    distribution,
    ## Assumption here is that sd is never zero, which might warrant
    ## special treatment (except that it's infinite so probably
    ## problematic anyway).
    normal = substitute(
      - (x - mu)^2 / (2 * sd^2) - log(sqrt(2 * pi)) - log(sd),
      list(x = target, mu = args[[1]], sd = args[[2]])),
    poisson = substitute(
      x * log(lambda) - lambda - lfactorial(x),
      list(x = target, lambda = args[[1]])),
    uniform = substitute(
      if (x < a || x > b) -Inf else -log(b - a),
      list(x = target, a = args[[1]], b = args[[2]])),
    stop(sprintf("Unsupported distribution '%s'", distribution)))
}
