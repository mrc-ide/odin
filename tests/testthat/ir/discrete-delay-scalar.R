r <- 3.6
update(y[]) <- r * y[i] * (1 - y[i])
initial(y[1]) <- 0.2
initial(y[2]) <- 0.4
x <- delay(sum(y) / length(y), 2)
output(x) <- TRUE
dim(y) <- 2
