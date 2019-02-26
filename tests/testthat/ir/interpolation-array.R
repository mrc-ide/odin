deriv(y[]) <- pulse[i]
initial(y[]) <- 0
pulse[] <- interpolate(tp, zp, "constant")
tp[] <- user()
zp[,] <- user()
dim(tp) <- user()
dim(zp) <- user()
dim(pulse) <- 2
dim(y) <- 2
