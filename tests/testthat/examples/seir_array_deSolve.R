seir_array <- function() {
  b <- 1 / (365 * 50)
  N <- 1e7
  beta <- 1
  sigma <- 1 / 30
  delta <- 1 / 60
  lat_hum <- 14
  I0 <- 1

  N_age <- 5L
  age_width <- c(1, 4, 10, 15, 20) * 365
  age_rate <- c(1 / age_width[-N_age], 0.0)
  den <- numeric(N_age)
  den[[1L]] <- 1.0 / (1.0 + age_rate[[1L]] / b)
  for (i in 2:N_age) {
    den[i] <- age_rate[[i - 1L]] * den[[i - 1L]] / (age_rate[i] + b)
  }
  rm(i)

  Births <- N*b
  ## i.e. proportion of humans surviving the latent period
  surv <- exp(-b*lat_hum)

  t0 <- NULL
  y0 <- NULL
  lag <- NULL

  i_S <- 1:N_age
  i_I <- 1:N_age + (2 * N_age)

  initial <- function(t = 0, pars = NULL) {
    if ("I0" %in% names(pars)) {
      I0 <<- pars$I0
    }
    t0 <<- t
    y0 <<- c(den * (N - I0), # S
             den * 0,        # E
             den * I0,       # I
             den * 0)        # R
    lag <<- make_lagvalue(t0, y0)
    y0
  }

  derivs <- function(t, y, .) {
    y <- matrix(y, N_age, 4L)

    S <- y[, 1L]
    E <- y[, 2L]
    I <- y[, 3L]
    R <- y[, 4L]
    dSdt <- numeric(N_age)
    dEdt <- numeric(N_age)
    dIdt <- numeric(N_age)
    dRdt <- numeric(N_age)

    I_tot <- sum(I)

    ## people developing latent infection
    new_inf <- beta * S * I_tot / N

    ## people that become latent 'lat_hum' days ago, less those that
    ## died during that time
    S_lag <- lag(t, lat_hum, i_S)
    I_lag <- lag(t, lat_hum, i_I)
    I_lag_tot <- sum(I_lag)
    lag_inf <- (beta * S_lag * I_lag_tot / N) * surv

    dSdt[[1]] <- - new_inf[[1L]] + delta * R[[1L]] - b * S[[1L]] +
      (Births - age_rate[[1L]] * S[[1L]])
    dSdt[-1] <- - new_inf[-1L] + delta * R[-1L] - b * S[-1L] +
      (age_rate[-N_age] * S[-N_age] - age_rate[-1L]*S[-1L])

    dEdt[[1L]] <- new_inf[[1L]] - lag_inf[[1L]] - b * E[[1L]] + (- age_rate[[1L]] * E[[1L]])
    dEdt[-1L] <- new_inf[-1L] - lag_inf[-1L] - b * E[-1L] + (age_rate[-N_age] * E[-N_age] - age_rate[-1L] * E[-1L])

    dIdt[[1L]] <- lag_inf[[1L]] - (b + sigma) * I[[1L]] +
      (- age_rate[[1L]] * I[[1L]])
    dIdt[-1L] <- lag_inf[-1L] - (b + sigma) * I[-1L] +
      (age_rate[-N_age] * I[-N_age] - age_rate[-1L] * I[-1L])

    dRdt[[1L]] <- sigma * I[[1L]] - b * R[[1L]] - delta * R[[1L]] +
      (- age_rate[[1L]] * R[[1L]])
    dRdt[-1L] <- sigma * I[-1L] - b * R[-1L] - delta * R[-1L] +
      (age_rate[-N_age] * R[-N_age] - age_rate[-1L] * R[-1L])

    list(c(dSdt, dEdt, dIdt, dRdt))
  }

  list(initial = initial, derivs = derivs, delay = TRUE, t = c(0, 365))
}
