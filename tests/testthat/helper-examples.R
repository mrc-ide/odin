## Update these as more models are added.
ODIN_TO_TEST <- c("lorenz", "sir", "seir", "array", "array_2d", "seir_array")

source1 <- function(filename) {
  x <- source(filename, local = TRUE)
  if (!is.function(x$value)) {
    stop("Did not get expected output from source")
  }
  x$value()
}

seq_range <- function(t, length.out) {
  seq(t[[1L]], t[[2L]], length.out = length.out)
}

## Lagvalue with the same semantics as BM; if a positive time is used
## then we'll get the lagged value.  Otherwise take the value from the
## initial conditions (y0).
make_lagvalue <- function(t0, y0) {
  force(t0)
  y0 <- unname(y0)
  function(t, lag, nr = 0L) {
    t1 <- t - lag
    if (t1 > t0) {
      deSolve::lagvalue(t1, nr)
    } else if (length(nr) == 1 && nr == 0L) {
      y0
    } else {
      y0[nr]
    }
  }
}
