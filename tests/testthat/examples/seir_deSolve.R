seir <- function() {
  b <- 1 / 10
  N <- 1e7
  beta <- 10
  sigma <- 1 / 3
  delta <- 1 / 21
  lat_hum <- 14
  I0 <- 1

  Births <- N*b
  ## i.e. proportion of humans surviving the latent period
  surv <- exp(-b*lat_hum)

  t0 <- NULL
  y0 <- NULL
  lag <- NULL

  initial <- function(t = 0, pars = NULL) {
    if ("I0" %in% names(pars)) {
      I0 <<- pars$I0
    }
    t0 <<- t
    y0 <<- c(S = N - I0,E = 0, I = I0, R = 0)
    lag <<- make_lagvalue(t0, y0)
    y0
  }

  derivs <- function(t, y, .) {
    S <- y[[1L]]
    E <- y[[2L]]
    I <- y[[3L]]
    R <- y[[4L]]

    ## people developing latent infection
    new_inf <- beta*S*I/N

    ## people that become latent 'lat_hum' days ago, less those that
    ## died during that time
    S_lag <- lag(t, lat_hum, 1L)
    I_lag <- lag(t, lat_hum, 3L)
    lag_inf <- S_lag * I_lag * beta * surv / N

    dS <- Births  - b * S - new_inf + delta * R
    dE <- new_inf - lag_inf - b * E
    dI <- lag_inf - (b + sigma) * I
    dR <- sigma * I - b * R - delta * R

    list(c(dS, dE, dI, dR))
    ## Output variables
    ## c(prev = I/N, Hpop = S+E+I+R))
  }

  list(initial = initial, derivs = derivs, delay = TRUE, t = c(0, 365))
}
