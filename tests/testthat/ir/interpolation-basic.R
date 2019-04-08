deriv(y) <- pulse
initial(y) <- 0
pulse <- interpolate(tp, zp, "constant")
tp[] <- user()
zp[] <- user()
dim(tp) <- user()
dim(zp) <- user()
output(p) <- pulse
