deriv(a[]) <- i
initial(a[]) <- (i - 1) / 10
dim(a) <- 5
alt[] <- user()
dim(alt) <- length(a)
tmp[] <- delay(a[i], 2, alt[i])
dim(tmp) <- length(a)
output(tmp[]) <- TRUE
