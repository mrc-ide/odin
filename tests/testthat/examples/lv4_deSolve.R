lv4 <- function() {
  r <- NULL
  a <- NULL

  initial <- function(t = 0, pars = NULL) {
    r <<- pars[["r"]]
    a <<- pars[["a"]]
    pars[["y0"]]
  }

  derivs <- function(t, y, .) {
    ## Not much faster, less clear:
    ##   list(r * y * (1 - colSums(t(a) * y)))
    list(vapply(seq_along(y), function(i)
      r[i] * y[i] * (1 - sum(a[i, ] * y)), numeric(1)))
  }

  list(derivs = derivs, initial = initial, t = c(0, 100))
}
