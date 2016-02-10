source1 <- function(filename) {
  x <- source(filename, local=TRUE)
  if (!is.function(x$value)) {
    stop("Did not get expected output from source")
  }
  x$value()
}

seq_range <- function(t, length.out) {
  seq(t[[1L]], t[[2L]], length.out=length.out)
}
