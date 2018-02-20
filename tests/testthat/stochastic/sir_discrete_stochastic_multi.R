
update(S) <- S - n_SI
update(I) <- I + n_SI - n_IRD
update(R) <- R + n_IR
update(D) <- D + n_ID

initial(S) <- S0
initial(I) <- 10
initial(R) <- 0
initial(D) <- 0

p_SI <- 1 - exp(-beta * I / N)
p_IRD <- 1 - exp(-gamma)

n_SI <- rbinom(S, p_SI)

n_IRD <- rbinom(I, p_IRD)
## NOTE: rmultinom must be the only call on the rhs, lhs must be an
## array.  The p argument must be an array that is the same size as
## tmp, but this is not checked and will just happily crash if you get
## it wrong.
tmp[] <- rmultinom(n_IRD, p)
n_IR <- tmp[1]
n_ID <- tmp[2]

mu <- user(1)
p[1] <- mu
p[2] <- 1 - mu
dim(p) <- 2
dim(tmp) <- 2

S0 <- user(100)
beta <- user(0.1)
gamma <- user(0.1)

N <- S + I + R + D
output(N) <- TRUE
