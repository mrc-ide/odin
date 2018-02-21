
## equations for transitions between compartments

update(S[]) <- S[i] - n_SI[i]
update(I[]) <- I[i] + n_SI[i] - n_IR[i]
update(R[]) <- R[i] + n_IR[i]


## individual probabilities of transition

p_SI[] <- 1 - exp(-beta * I[i] / N[i])
p_IR <- 1 - exp(-gamma) # I to R


## draws from binomial distributions

n_SI[] <- rbinom(S[i], p_SI[i])
n_IR[] <- rbinom(I[i], p_IR)


## record total population size

N[] <- S[i] + I[i] + R[i]


## initial states

initial(S[]) <- S_ini # will be user-defined
initial(I[]) <- I_ini # will be user-defined
initial(R[]) <- 0

S_ini <- user(1000) # user-defined, default = 1000
I_ini <- user(1) # user-defined, default = 1
beta <- user(0.2) # user-defined, default =  0.2
gamma <- user(0.1) # user-defined, default = 0.1


## number of replicates

nsim <- user(100) # user-defined, default = 100
dim(N) <- nsim
dim(S) <- nsim
dim(I) <- nsim
dim(R) <- nsim
dim(p_SI) <- nsim
dim(n_SI) <- nsim
dim(n_IR) <- nsim
