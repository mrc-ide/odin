v1 <- exp(-t)
initial(y1) <- 1
deriv(y1) <- y1 * v1
deriv(y2) <- y2 * 0.5
initial(y2) <- y1 + v1
deriv(y3) <- y3 * 0.1
initial(y3) <- y1 + y2
