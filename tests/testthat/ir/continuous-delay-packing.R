deriv(a[]) <- i
deriv(b[]) <- i
deriv(c[]) <- i
deriv(d[]) <- i
deriv(e[]) <- i

initial(a[]) <- 1
initial(b[]) <- 2
initial(c[]) <- 3
initial(d[]) <- 4
initial(e[]) <- 5

foo[] <- delay(b[i] + c[i + 1] + e[i + 2], 2)
output(foo[]) <- TRUE

dim(foo) <- 9
dim(a) <- 10
dim(b) <- 11
dim(c) <- 12
dim(d) <- 13
dim(e) <- 14
