## Exponential growth/decay of 'y'
deriv(y[]) <- r[i] * y[i]
initial(y[]) <- y0[i]
r[] <- user()

## Drive the system off of given 'y0'
y0[] <- user()
dim(y0) <- user()
dim(y) <- length(y0)
dim(r) <- length(y0)

y2[] <- y[i] * y[i]
total2 <- sum(y2)
dim(y2) <- length(y0)

## Delay the total of all variables
a <- delay(total2, 2.5)

## And output that for checking
output(a) <- a
