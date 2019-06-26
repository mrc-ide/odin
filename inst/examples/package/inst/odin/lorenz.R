deriv(y1) <- sigma * (y2 - y1)
deriv(y2) <- R * y1 - y2 - y1 * y3
deriv(y3) <- -b * y3 + y1 * y2

initial(y1) <- 10.0
initial(y2) <- 1.0
initial(y3) <- 1.0

## These are the classical parameters:
sigma <- 10
R <- 28
b <-  8 / 3
