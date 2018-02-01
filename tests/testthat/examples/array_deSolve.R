age <- function() {
  b <- 1 / (365 * 50)
  N <- 1e7
  I0 <- 1
  Births <- b * N
  beta <- 1
  sigma <- 1 / 30
  delta <- 1 / 60

  N_age <- 5L
  age_width <- c(1, 4, 10, 15, 20) * 365
  age_rate <- c(1 / age_width[-N_age], 0.0)

  ## to work out the % of the population in each age group
  den <- numeric(N_age)
  den[[1L]] <- 1.0 / (1.0 + age_rate[[1L]] / b)
  for (i in 2:N_age) {
    den[i] <- age_rate[[i - 1L]] * den[[i - 1L]] / (age_rate[i] + b)
  }

  initial <- function(t = 0, pars = NULL) {
    if ("I0" %in% names(pars)) {
      I0 <<- pars$I0
    }
    S0 <- den * (N - I0)
    I0 <- den * I0
    R0 <- den * 0
    ret <- c(S0, I0, R0)
    attr(ret, "output_len") <- 2L
    ret
  }

  derivs <- function(t, y, .) {
    y <- matrix(y, N_age, 3L)
    S <- y[, 1L]
    I <- y[, 2L]
    R <- y[, 3L]
    dSdt <- numeric(N_age)
    dIdt <- numeric(N_age)
    dRdt <- numeric(N_age)
    I_tot <- sum(I)

    dSdt[[1L]] <- - beta * S[[1L]] * I_tot/N + delta * R[[1L]] - b * S[[1L]] + (Births - age_rate[[1L]] * S[[1L]])
    dSdt[-1L] <- - beta * S[-1L] * I_tot/N + delta * R[-1L] - b * S[-1L] + (age_rate[-N_age] * S[-N_age] - age_rate[-1L] * S[-1L])

    dIdt[[1L]] <-  beta * S[[1L]] * I_tot/N  - (b+sigma) * I[[1L]]   + (- age_rate[[1L]] * I[[1L]])
    dIdt[-1L] <-  beta * S[-1L] * I_tot/N  - (b+sigma) * I[-1L]   + (age_rate[-N_age] * I[-N_age] - age_rate[-1L] * I[-1L])

    dRdt[[1L]] <-  sigma * I[[1L]] - b * R[[1L]]-delta * R[[1L]] + (- age_rate[[1L]] * R[[1L]])
    dRdt[-1L] <-  sigma * I[-1L] - b * R[-1L]-delta * R[-1L] + (age_rate[-N_age] * R[-N_age] - age_rate[-1L] * R[-1L])

    N_tot <- sum(S + I + R)
    prev <- I_tot / N_tot * 100
    list(c(dSdt, dIdt, dRdt),
         c(N_tot = N_tot, prev = prev))
  }

  list(derivs = derivs, initial = initial, t = c(0, 100))
}
