deriv(y[]) <- r[i] * y[i] * (1 - sum(ay[i, ]))
initial(y[]) <- y0[i]

y0[] <- user()
r[] <- user()
a[, ] <- user()
ay[, ] <- a[i, j] * y[j]

dim(r) <- user()
n_spp <- length(r)

dim(y) <- n_spp
dim(y0) <- n_spp
dim(a) <- c(n_spp, n_spp)
dim(ay) <- c(n_spp, n_spp)
