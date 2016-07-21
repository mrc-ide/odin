#include <interpolate.h>
#include <interpolate.c>
#include <Rinternals.h>

SEXP test_interpolate0(SEXP x, SEXP y, SEXP xout) {
  size_t n = (size_t)length(x), m = (size_t)length(xout);
  // NOTE: assumes transposed.
  size_t ny = isMatrix(y) ? nrows(y) : 1;
  interpolate_0_data *obj = interpolate_0_data_alloc(n, ny, REAL(x), REAL(y));
  SEXP ret;

  if (isMatrix(y)) {
    ret = PROTECT(allocMatrix(REALSXP, ny, length(xout)));
  } else {
    ret = PROTECT(allocVector(REALSXP, length(xout)));
  }
  for (size_t i = 0; i < m; ++i) {
    interpolate_0_data_run(REAL(xout)[i], obj, REAL(ret) + i * ny);
  }
  interpolate_0_data_free(obj);
  UNPROTECT(1);
  return ret;
}
