# nolint start (can't cope with uppercase names here)
dt <- 1.0 / freq
p_IR <- 1 - exp(-(gamma) * dt)
S0 <- 1000
freq <- user(4)

N <- S + I + R
p_inf <- beta * I / N * dt
p_SI <- 1 - exp(-(p_inf))
n_SI <- rbinom(S, p_SI)
n_IR <- rbinom(I, p_IR)

update(S) <- S - n_SI
update(I) <- I + n_SI - n_IR
update(R) <- R + n_IR
update(cases_cumul) <- cases_cumul + n_SI
update(cases_inc) <- if (step %% freq == 0) n_SI else cases_inc + n_SI

initial(S) <- S0
initial(I) <- I0
initial(R) <- 0
initial(cases_cumul) <- 0
initial(cases_inc) <- 0

beta <- user(0.2, differentiate = TRUE)
gamma <- user(0.1, differentiate = TRUE)
I0 <- user(10, differentiate = TRUE)

cases_observed <- data()
compare(cases_observed) ~ poisson(cases_inc)

config(base) <- "sir"
# nolint end
