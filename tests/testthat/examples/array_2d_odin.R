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

initial(y[1:N_age, 1]) <- den[i] * (N - I0)
initial(y[1:N_age, 2]) <- den[i] * I0
initial(y[1:N_age, 3]) <- 0

I0 <- user(1)

Births <- b * N
b <- 1 / (365 * 50)
N <- 1e7
beta <- 1
sigma <- 1 / 30
delta <- 1 / 60

I_tot <- sum(y[, 2])

deriv(y[1, 1]) <- - beta * y[i, 1] * I_tot / N + delta * y[i, 3] - b * y[i, 1] +
  (Births - age_rate[i] * y[i, 1])
deriv(y[2:N_age, 1]) <- - beta * y[i, 1] * I_tot / N + delta * y[i, 3] -
  b * y[i, 1] + (age_rate[i - 1] * y[i - 1, 1] - age_rate[i] * y[i, 1])

deriv(y[1, 2]) <- beta * y[i, 1] * I_tot / N - (b + sigma) * y[i, 2] +
  (- age_rate[i] * y[i, 2])
deriv(y[2:N_age, 2]) <- beta * y[i, 1] * I_tot / N - (b + sigma) * y[i, 2] +
  (age_rate[i - 1] * y[i - 1, 2] - age_rate[i] * y[i, 2])

deriv(y[1, 3]) <- sigma * y[i, 2] - b * y[i, 3] - delta * y[i, 3] +
  (- age_rate[i] * y[i, 3])
deriv(y[2:N_age, 3]) <- sigma * y[i, 2] - b * y[i, 3] - delta * y[i, 3] +
  (age_rate[i - 1] * y[i - 1, 3] - age_rate[i] * y[i, 3])

## TODO: Can I get a nicer syntax here (for the N_tot case
##
## NOTE: For this sort of output variable, where things are simply
## computed from the core variables, post-processing will tend to be
## preferable I suspect.
N_tot <- sum(y)
output(N_tot) <- N_tot
output(prev) <- I_tot / N_tot * 100

dim(den) <- N_age
dim(age_width) <- N_age
dim(age_rate) <- N_age
dim(y) <- c(N_age, 3)
