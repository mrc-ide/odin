#include <interpolate.c>
#include <Rinternals.h>

SEXP test_interpolate(SEXP x, SEXP y, SEXP xout, SEXP type) {
  size_t n = (size_t)length(x), m = (size_t)length(xout);
  size_t ny = isMatrix(y) ? ncols(y) : 1;

  interpolate_data *obj = interpolate_alloc(INTEGER(type)[0],
                                            n, ny, REAL(x), REAL(y));
  SEXP ret;

  if (isMatrix(y)) {
    ret = PROTECT(allocMatrix(REALSXP, length(xout), ny));
  } else {
    ret = PROTECT(allocVector(REALSXP, length(xout)));
  }

  double *tmp = (double*) R_alloc(ny, sizeof(double));
  double *yout = REAL(ret);
  for (size_t i = 0; i < m; ++i) {
    if (obj->type == 0) {
      interpolate_0_run(REAL(xout)[i], obj, tmp);
    } else if (obj->type == 1) {
      interpolate_1_run(REAL(xout)[i], obj, tmp);
    } else if (obj->type == 2) {
      interpolate_2_run(REAL(xout)[i], obj, tmp);
    } else {
      Rf_error("Invalid interpolation type");
    }
    for (size_t j = 0; j < ny; ++j) {
      yout[i + j * m] = tmp[j];
    }
  }

  interpolate_free(obj);
  UNPROTECT(1);
  return ret;
}
