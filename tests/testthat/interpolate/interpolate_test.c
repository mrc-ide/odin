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
    switch(obj->type) {
    case CONSTANT:
      interpolate_constant_run(REAL(xout)[i], obj, tmp);
      break;
    case LINEAR:
      interpolate_linear_run(REAL(xout)[i], obj, tmp);
      break;
    case SPLINE:
      interpolate_spline_run(REAL(xout)[i], obj, tmp);
      break;
    default:
      Rf_error("Invalid interpolation type");
      break;
    }
    for (size_t j = 0; j < ny; ++j) {
      yout[i + j * m] = tmp[j];
    }
  }

  interpolate_free(obj);
  UNPROTECT(1);
  return ret;
}
