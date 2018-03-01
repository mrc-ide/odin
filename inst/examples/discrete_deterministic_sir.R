## Core equations for transitions between compartments:
update(S) <- S - beta * S * I / N
update(I) <- I + beta * S * I / N - gamma * I
update(R) <- R + gamma * I

## Total population size (odin will recompute this at each timestep:
## automatically)
N <- S + I + R

## Initial states:
initial(S) <- S_ini # will be user-defined
initial(I) <- I_ini # will be user-defined
initial(R) <- 0

## User defined parameters - default in parentheses:
S_ini <- user(1000)
I_ini <- user(1)
beta <- user(0.2)
gamma <- user(0.1)
