initial(S) <- N - 1
initial(E) <- 0
initial(I) <-1
initial(R) <- 0

Births <- b * N
b <- 1 / 10
N <- 1e7
beta <- 10
sigma <- 1 / 3
delta <- 1 / 21
lat_hum <- 14

## people developing latent infection
new_inf <- beta * S * I / N
## i.e. proportion of humans surviving the latent period
surv <- exp(-b * lat_hum)
## people that become latent 'lath_um' days ago, less those that died
## during that time
lag_inf <- delay(new_inf  *  surv, lat_hum)

deriv(S) <- Births - b * S - beta * S * I / N+delta * R
deriv(E) <- new_inf - lag_inf - b * E
deriv(I) <- lag_inf  - (b + sigma) * I
deriv(R) <- sigma * I - b * R - delta * R

Ntot <- S + E + I + R
prev <- I / Ntot * 100

output(prev) <- I / N
output(Hpop) <- S + E + I + R
