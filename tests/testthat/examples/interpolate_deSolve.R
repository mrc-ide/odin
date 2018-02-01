interpolate <- function() {
  flux <- NULL
  k <- NULL
  C0 <- NULL
  initial <- function(t = 1, pars = NULL) {
    flux <<- approxfun(pars$flux_t, pars$flux_y)
    k <<- pars$k
    C0 <<- mean(flux(1:365)) / k
    C0
  }

  derivs <- function(t, y, .) {
    flux_t <- flux(t)
    list(flux_t - k * y)
  }

  list(derivs = derivs, initial = initial, t = c(1, 365))
}
