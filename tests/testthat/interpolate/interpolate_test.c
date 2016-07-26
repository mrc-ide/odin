#include <interpolate.c>
#include <Rinternals.h>

SEXP test_interpolate0(SEXP x, SEXP y, SEXP xout, SEXP order) {
  size_t n = (size_t)length(x), m = (size_t)length(xout);
  size_t ny = isMatrix(y) ? ncols(y) : 1;
  interpolate_0_data *obj = interpolate_0_alloc(n, ny, REAL(x), REAL(y));
  SEXP ret;

  if (isMatrix(y)) {
    ret = PROTECT(allocMatrix(REALSXP, length(xout), ny));
  } else {
    ret = PROTECT(allocVector(REALSXP, length(xout)));
  }

  double *tmp = (double*) R_alloc(ny, sizeof(double));
  double *yout = REAL(ret);
  for (size_t i = 0; i < m; ++i) {
    if (INTEGER(order)[0] == 0) {
      interpolate_0_run(REAL(xout)[i], obj, tmp);
    } else {
      interpolate_1_run(REAL(xout)[i], obj, tmp);
    }
    for (size_t j = 0; j < ny; ++j) {
      yout[i + j * m] = tmp[j];
    }
  }

  interpolate_0_free(obj);
  UNPROTECT(1);
  return ret;
}
