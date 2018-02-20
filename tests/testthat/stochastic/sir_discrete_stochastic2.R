update(S[]) <- S[i] - n_SI[i]
update(I[]) <- I[i] + n_SI[i] - n_IR[i]
update(R[]) <- R[i] + n_IR[i]

initial(S[]) <- S0
initial(I[]) <- 10
initial(R[]) <- 0

p_SI[] <- 1 - exp(-beta * I[i] / N[i])
p_IR <- 1 - exp(-gamma)

n_SI[] <- rbinom(S[i], p_SI[i])
n_IR[] <- rbinom(I[i], p_IR)

S0 <- user(100)
beta <- user(0.1)
gamma <- user(0.1)

N[] <- S[i] + I[i] + R[i]

nsim <- user(100)
dim(N) <- nsim
dim(S) <- nsim
dim(I) <- nsim
dim(R) <- nsim
dim(p_SI) <- nsim
dim(n_SI) <- nsim
dim(n_IR) <- nsim
