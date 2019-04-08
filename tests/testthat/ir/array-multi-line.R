initial(x) <- 1
deriv(x) <- 1
a[1] <- 1
a[2] <- 1
a[3:n] <- a[i - 1] + a[i - 2]
dim(a) <- n
n <- 10
