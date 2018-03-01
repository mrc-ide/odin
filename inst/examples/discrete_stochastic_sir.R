## Core equations for transitions between compartments:
update(S) <- S - n_SI
update(I) <- I + n_SI - n_IR
update(R) <- R + n_IR

## Individual probabilities of transition:
p_SI <- 1 - exp(-beta * I / N) # S to I
p_IR <- 1 - exp(-gamma) # I to R

## Draws from binomial distributions for numbers changing between
## compartments:
n_SI <- rbinom(S, p_SI)
n_IR <- rbinom(I, p_IR)

## Total population size
N <- S + I + R

## Initial states:
initial(S) <- S_ini
initial(I) <- I_ini
initial(R) <- 0

## User defined parameters - default in parentheses:
S_ini <- user(1000)
I_ini <- user(1)
beta <- user(0.2)
gamma <- user(0.1)
