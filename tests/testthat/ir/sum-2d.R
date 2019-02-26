deriv(y) <- 0
initial(y) <- 1

a[,,] <- user()
dim(a) <- user()

## These collapse one dimension
m12[,] <- sum(a[i, j, ])
m13[,] <- sum(a[i, , j])
m23[,] <- sum(a[, i, j])

dim(m12) <- c(dim(a, 1), dim(a, 2))
dim(m13) <- c(dim(a, 1), dim(a, 3))
dim(m23) <- c(dim(a, 2), dim(a, 3))

## These collapse two dimensions
v1[] <- sum(a[i, , ])
v2[] <- sum(a[, i, ])
v3[] <- sum(a[, , i])
dim(v1) <- dim(a, 1)
dim(v2) <- dim(a, 2)
dim(v3) <- dim(a, 3)

mm12[,] <- sum(a[i, j, 2:4])
mm13[,] <- sum(a[i, 2:4, j])
mm23[,] <- sum(a[2:4, i, j])
## TODO: dim(mm12) <- dim(m12) will not work, but that would be nice
dim(mm12) <- c(dim(a, 1), dim(a, 2))
dim(mm13) <- c(dim(a, 1), dim(a, 3))
dim(mm23) <- c(dim(a, 2), dim(a, 3))

vv1[] <- sum(a[i, 2:4, 2:4])
vv2[] <- sum(a[2:4, i, 2:4])
vv3[] <- sum(a[2:4, 2:4, i])
dim(vv1) <- dim(a, 1)
dim(vv2) <- dim(a, 2)
dim(vv3) <- dim(a, 3)

tot1 <- sum(a)
tot2 <- sum(a[,,])
