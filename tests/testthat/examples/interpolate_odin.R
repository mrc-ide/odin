deriv(C) <- flux - kk * C
initial(C) <- C0
flux <- interpolate(flux_t, flux_y, "linear")
C0 <- user()
kk <- user()
output(deposition) <- kk * C
flux_t[] <- user()
flux_y[] <- user()
dim(flux_t) <- user()
dim(flux_y) <- user()
