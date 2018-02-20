update(S) <- S - beta * S * I / N
update(I) <- I + beta * S * I / N - gamma * I
update(R) <- R + gamma * I

initial(S) <- S0
initial(I) <- 10
initial(R) <- 0

S0 <- user(100)
beta <- user(0.1)
gamma <- user(0.1)

N <- S + I + R
