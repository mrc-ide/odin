## Exponential growth/decay of 'y'
deriv(y[]) <- r[i] * y[i]
initial(y[]) <- y0[i]
r[] <- user()

## Drive the system off of given 'y0'
y0[] <- user()
dim(y0) <- user()
dim(y) <- length(y0)
dim(r) <- length(y0)

## And of a scalar 'z'
deriv(z) <- rz * z
initial(z) <- 1
rz <- 0.1

total <- sum(y) + z

## Delay the total of all variables
a <- delay(total, 2.5)
output(a) <- a
