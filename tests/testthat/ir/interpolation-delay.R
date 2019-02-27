deriv(y) <- ud
initial(y) <- 0
deriv(z) <- u
initial(z) <- 0
output(u) <- TRUE
output(ud) <- TRUE

u <- interpolate(ut, uy, "linear")

ud <- delay(u, 2)

ut[] <- user()
uy[] <- user()
dim(ut) <- user()
dim(uy) <- length(ut)
