
## equations for transitions between compartments

update(S) <- S - n_SI
update(I) <- I + n_SI - n_IR
update(R) <- R + n_IR


## individual probabilities of transition

p_SI <- 1 - exp(-beta * I / N) # S to I
p_IR <- 1 - exp(-gamma) # I to R


## draws from binomial distributions

n_SI <- rbinom(S, p_SI)
n_IR <- rbinom(I, p_IR)


## record total population size

N <- S + I + R


## initial states

initial(S) <- S_ini # will be user-defined
initial(I) <- I_ini # will be user-defined
initial(R) <- 0

S_ini <- user(1000) # user-defined, default = 1000
I_ini <- user(1) # user-defined, default = 1
beta <- user(0.2) # user-defined, default =  0.2
gamma <- user(0.1) # user-defined, default = 0.1

