sir <- function() {
  N <- 1e7
  Births <- N / 75
  b <- 1 / 75
  I0 <- 1
  beta <- 24
  sigma <- 12
  delta <- 1 / 5

  initial <- function(t = 0, pars = NULL) {
    if ("I0" %in% names(pars)) {
      I0 <<- pars$I0
    }
    c(N - I0, I0, 0.0)
  }

  derivs <- function(t, y, .) {
    S <- y[[1L]]
    I <- y[[2L]]
    R <- y[[3L]]
    list(c(Births - b * S - beta * S * I / N + delta * R,
           beta * S * I / N - (b + sigma) * I,
           sigma * I - b * R - delta * R))
  }

  list(derivs = derivs, initial = initial, t = c(0, 100))
}
