deriv(S) <- Births - b * S - beta * S * I / N + delta * R
deriv(I) <- beta * S * I / N - (b + sigma) * I
deriv(R) <- sigma * I - b * R - delta * R

initial(S) <- N - I0
initial(I) <- I0
initial(R) <- 0

Births <- N / 75
b <- 1 / 75
N <- 1e7
I0 <- user(1)
beta <- user(24)
sigma <- 12
delta <- 1 / 5

config(base) <- "sir"
