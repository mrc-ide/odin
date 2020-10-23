N_age <- 5
## mean no. of days in each age compartment (0 - 1 yr, 1 - 5 yr, 5 -
## 15 yr, 15 - 30yr, 30 +)
age_width[1] <- 365 * 1
age_width[2] <- 365 * 4
age_width[3] <- 365 * 10
age_width[4] <- 365 * 15
age_width[5] <- 365 * 20

age_rate[1:(N_age - 1)] <- 1 / age_width[i]
age_rate[N_age] <- 0

den[1] <- 1 / (1 + age_rate[1] / b)
## to work out the % of the population in each age group
den[2:N_age] <- age_rate[i - 1] * den[i - 1] / (age_rate[i] + b)

initial(S[1:N_age]) <- den[i] * (N - I0)
initial(E[1:N_age]) <- 0
initial(I[1:N_age]) <- den[i] * I0
initial(R[1:N_age]) <- 0

I0 <- user(1)

Births <- b * N
b <- 1 / (365 * 50)
N <- 1e7
beta <- 1
sigma <- 1 / 30
delta <- 1 / 60
lat_hum <- 14

I_tot <- sum(I)

surv <- exp(-b * lat_hum)
new_inf[] <- beta * S[i] * I_tot / N
lag_inf[] <- delay(new_inf[i] * surv, lat_hum)

deriv(S[1]) <- - new_inf[i] + delta * R[i] - b * S[i] +
  (Births - age_rate[i] * S[i])
deriv(S[2:N_age]) <- - new_inf[i] + delta * R[i] - b * S[i] +
  (age_rate[i - 1] * S[i - 1] - age_rate[i] * S[i])

deriv(E[1]) <- new_inf[i] - lag_inf[i] - b * E[i] + (- age_rate[i] * E[i])
deriv(E[2:N_age]) <- new_inf[i] - lag_inf[i] - b * E[i] +
  (age_rate[i - 1] * E[i - 1] - age_rate[i] * E[i])

deriv(I[1]) <- lag_inf[i] - (b + sigma) * I[i] +
  (- age_rate[i] * I[i])
deriv(I[2:N_age]) <- lag_inf[i] - (b + sigma) * I[i] +
  (age_rate[i - 1] * I[i - 1] - age_rate[i] * I[i])

deriv(R[1]) <- sigma * I[i] - b * R[i] - delta * R[i] +
  (- age_rate[i] * R[i])
deriv(R[2:N_age]) <- sigma * I[i] - b * R[i] - delta * R[i] +
  (age_rate[i - 1] * R[i - 1] - age_rate[i] * R[i])

dim(den) <- N_age
dim(age_width) <- N_age
dim(age_rate) <- N_age
dim(S) <- N_age
dim(E) <- N_age
dim(I) <- N_age
dim(R) <- N_age

dim(new_inf) <- N_age
dim(lag_inf) <- N_age
