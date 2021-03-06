age_width[] <- user()
dim(age_width) <- user()

N_age <- length(age_width)

age_rate[1:(N_age - 1)] <- 1 / age_width[i]
age_rate[N_age] <- 0

den[1] <- 1 / (1 + age_rate[1] / b)
## to work out the % of the population in each age group
den[2:N_age] <- age_rate[i - 1] * den[i - 1] / (age_rate[i] + b)

initial(S[1:N_age]) <- den[i] * (N - I0)
initial(I[1:N_age]) <- den[i] * I0
initial(R[1:N_age]) <- 0

I0 <- user(1)

Births <- b * N
b <- 1 / (365 * 50)
N <- 1e7
beta <- 1
sigma <- 1 / 30
delta <- 1 / 60

I_tot <- sum(I)

deriv(S[1]) <- - beta * S[i] * I_tot / N + delta * R[i] - b * S[i] +
  (Births - age_rate[i] * S[i])
deriv(S[2:N_age]) <- - beta * S[i] * I_tot / N + delta * R[i] - b * S[i] +
  (age_rate[i - 1] * S[i - 1] - age_rate[i] * S[i])

deriv(I[1]) <- beta * S[i] * I_tot / N - (b + sigma) * I[i] +
  (- age_rate[i] * I[i])
deriv(I[2:N_age]) <- beta * S[i] * I_tot / N - (b + sigma) * I[i] +
  (age_rate[i - 1] * I[i - 1] - age_rate[i] * I[i])

deriv(R[1]) <- sigma * I[i] - b * R[i] - delta * R[i] +
  (- age_rate[i] * R[i])
deriv(R[2:N_age]) <- sigma * I[i] - b * R[i] - delta * R[i] +
  (age_rate[i - 1] * R[i - 1] - age_rate[i] * R[i])

## TODO: Can I get a nicer syntax here (for the N_tot case
##
## NOTE: For this sort of output variable, where things are simply
## computed from the core variables, post-processing will tend to be
## preferable I suspect.
N_tot <- sum(S) + sum(I) + sum(R)
output(N_tot) <- N_tot
output(prev) <- I_tot / N_tot * 100

dim(den) <- N_age
dim(age_rate) <- N_age
dim(S) <- N_age
dim(I) <- N_age
dim(R) <- N_age
