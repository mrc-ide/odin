## equations for transitions between compartments

update(S) <- S - n_SE + n_RS
update(E) <- E + n_SE - n_EI + n_import_E
update(Ir) <- Ir + n_EIr - n_IrR
update(Id) <- Id + n_EId - n_IdD
update(R) <- R + n_IrR
update(D) <- D + n_IdD


## individual probabilities of transition

p_SE <- 1 - exp(-beta * I / N)
p_EI <-  1 - exp(-delta)
p_IrR <- 1 - exp(-gamma_R) # Ir to R
p_IdD <- 1 - exp(-gamma_D) # Id to d
p_RS <- 1 - exp(-omega) # R to S


## draws from various distributions

n_SE <- rbinom(S, p_SE)
n_EI <- rbinom(E, p_EI)

n_EIrId[] <- rmultinom(n_EI, p)
p[1] <- 1 - mu
p[2] <- mu
dim(p) <- 2
dim(n_EIrId) <- 2
n_EIr <- n_EIrId[1]
n_EId <- n_EIrId[2]
n_IrR <- rbinom(Ir, p_IrR)
n_IdD <- rbinom(Id, p_IdD)

n_RS <- rbinom(R, p_RS)

n_import_E <- rpois(epsilon)


## record total population size

I <- Ir + Id
N <- S + E + Ir + Id + R + D


## initial states

initial(S) <- S_ini # will be user-defined
initial(E) <- E_ini # will be user-defined
initial(Id) <- 0
initial(Ir) <- 0
initial(R) <- 0
initial(D) <- 0

S_ini <- user(1000) # susceptibles
E_ini <- user(1) # infected
beta <- user(0.2) # infection rate
delta <- user(0.3) # inverse incubation
gamma_R <- user(0.08) # recovery rate
gamma_D <- user(0.12) # death rate
mu <- user(0.7) # CFR
omega <- user(0.02) # rate of waning immunity
epsilon <- user(0.1) # import case rate



## ## number of replicates

## nsim <- user(100) # user-defined, default = 100
## dim(N) <- nsim
## dim(S) <- nsim
## dim(I) <- nsim
## dim(R) <- nsim
## dim(p_SI) <- nsim
## dim(n_SI) <- nsim
## dim(n_IR) <- nsim
