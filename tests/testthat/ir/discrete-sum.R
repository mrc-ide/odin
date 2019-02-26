initial(x[]) <- x0[i]
update(x[]) <- x[i] + r[i]
x0[] <- user()
r[] <- user()
dim(x0) <- user()
dim(x) <- length(x0)
dim(r) <- length(x)
output(total) <- sum(x)
