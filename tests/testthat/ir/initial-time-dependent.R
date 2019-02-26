y1 <- cos(t)
y2 <- y1 * (r + t)
r <- 1
deriv(y3) <- y2
initial(y3) <- y2
