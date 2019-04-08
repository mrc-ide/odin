initial(y[, ]) <- 1
update(y[, ]) <- y[i, j] + 1

initial(z[, ]) <- 1
update(z[, ]) <- a[i, j]

a[, ] <- delay(y[i, j], 2)
dim(y) <- c(2, 3)
dim(z) <- c(2, 3)
dim(a) <- c(2, 3)
