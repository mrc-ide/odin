config(include) <- "user_fns.c"
z <- squarepulse(t, 1, 2)
output(z) <- z
deriv(y) <- z
initial(y) <- 0
