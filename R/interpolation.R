##' Interface to the same interpolation functions as odin uses.  This
##' is primarily going to be useful for testing so that you can see
##' exactly how odin interpolates functions.
##' @title Interpolation
##'
##' @param x Independent variable (time in an actual odin model)
##'
##' @param y Dependent variable (vector or matrix)
##'
##' @param type Character string indicating the interpolation type
##'   ("constant", "linear" or "spline").
##'
##' @export
##' @useDynLib odin, .registration = TRUE
interpolation_function <- function(x, y, type) {
  is_matrix <- is.matrix(y)
  if (is_matrix) {
    ny <- ncol(y)
  } else {
    y <- matrix(y, ncol = 1)
    ny <- 1L
  }
  types <- c("constant", "linear", "spline")
  type_idx <- match(type, types) - 1L
  if (is.na(type_idx)) {
    stop(sprintf("type '%s' not known", type))
  }
  ptr <- .Call(Cinterpolate_prepare, x, y, type_idx)
  function(x) {
    y <- .Call(Cinterpolate_eval, ptr, x)
    if (is_matrix) {
      matrix(y, ncol = ny, byrow = TRUE)
    } else {
      y
    }
  }
}
