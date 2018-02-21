
## equations for transitions between compartments

update(S) <- S - beta * S * I / N
update(I) <- I + beta * S * I / N - gamma * I
update(R) <- R + gamma * I


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

