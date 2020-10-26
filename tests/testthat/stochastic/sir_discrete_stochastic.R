update(S) <- S - n_SI
update(I) <- I + n_SI - n_IR
update(R) <- R + n_IR

initial(S) <- S0
initial(I) <- 10
initial(R) <- 0

p_SI <- 1 - exp(-beta * I / N)
p_IR <- 1 - exp(-gamma)

n_SI <- rbinom(S, p_SI)
n_IR <- rbinom(I, p_IR)

## If order of operations matter
## > S1 <- S + ....
## > update(S) <- S1

S0 <- user(100)
beta <- user(0.1)
gamma <- user(0.1)

N <- S + I + R
