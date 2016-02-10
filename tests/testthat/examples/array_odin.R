N_age <- 5
## mean no. of days in each age compartment (0 - 1 yr, 1 - 5 yr, 5 -
## 15 yr, 15 - 30yr, 30 +)
age_width[1] <- 365 * 1
age_width[2] <- 365 * 4
age_width[3] <- 365 * 10
age_width[4] <- 365 * 15
age_width[5] <- 365 * 20

age_rate[-N_age] <- 1 / age_width[i]
age_rate[N_age] <- 0

den[1] <- 1 / (1+age_rate[1] / b)
## to work out the % of the population in each age group
den[-1] <- age_rate[i - 1] * den[i - 1] / (age_rate[i] + b)

## test to make sure densities add up to 1
##   TODO: should do an assert in here?
den_tot <- sum(den)

initial(S[]) <- den[i] * (N - 1)
initial(I[]) <- den[i] * 1
initial(R[]) <- den[i] * 0

Births <- b * N
b <- 1 / (365 * 50)
N <- 1e7
beta <- 1
sigma <- 1 / 30
delta <- 1 / 60

I_tot <- sum(I)

deriv(S[1]) <- - beta * S[i] * I_tot / N + delta * R[i] - b * S[i] +
  (Births - age_rate[i] * S[i])
deriv(S[-1]) <- - beta * S[i] * I_tot / N + delta * R[i] - b * S[i] +
  (age_rate[i-1] * S[i-1] - age_rate[i]*S[i])

deriv(I[1]) <- beta * S[i] * I_tot / N - (b + sigma) * I[i] +
  (- age_rate[i] * I[i])
deriv(I[-1]) <- beta * S[i] * I_tot / N - (b + sigma) * I[i] +
  (age_rate[i-1] * I[i-1] - age_rate[i] * I[i])

deriv(R[1]) <- sigma * I[i] - b * R[i] - delta * R[i] +
  (- age_rate[i] * R[i])
deriv(R[-1]) <- sigma * I[i] - b * R[i] - delta * R[i] +
  (age_rate[i-1] * R[i-1] - age_rate[i] * R[i])

N_tot <- sum(S) + sum(I) + sum(R)
prev <- I_tot / N_tot * 100
