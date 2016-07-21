#include <interpolate.h>
#include <interpolate.c>
#include <Rinternals.h>

SEXP test_interpolate0(SEXP x, SEXP y, SEXP xout) {
  size_t n = (size_t)length(x), m = (size_t)length(xout);
  interpolate_0_data *obj = interpolate_0_data_alloc(n, REAL(x), REAL(y));
  SEXP ret = PROTECT(allocVector(REALSXP, length(xout)));
  for (size_t i = 0; i < m; ++i) {
    interpolate_0_data_run(REAL(xout)[i], obj, REAL(ret) + i);
  }
  interpolate_0_data_free(obj);
  UNPROTECT(1);
  return ret;
}
