logistic_map <- function(r, y, t) {
  ret <- matrix(0, t + 1, length(y))
  ret[1, ] <- y
  for (i in seq_len(t)) {
    ret[i + 1, ] <- y <- r * y * (1 - y)
  }
  ret
}
