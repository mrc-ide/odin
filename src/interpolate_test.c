#include "interpolate_test.h"
#include <interpolate.c>
#include <stdbool.h>

static void interpolate_data_finalize(SEXP extPtr);
interpolate_data* interpolate_data_get(SEXP r_ptr, bool closed_error);

SEXP interpolate_prepare(SEXP r_x, SEXP r_y, SEXP r_type) {
  interpolate_type type = INTEGER(r_type)[0];
  size_t n = (size_t)length(r_x), ny;
  double *x = REAL(r_x), *y = REAL(r_y);
  if (isMatrix(r_y)) {
    ny = INTEGER(getAttrib(r_y, R_DimSymbol))[1];
  } else {
    ny = 1;
  }
  if ((size_t)length(r_y) != ny * n) {
    Rf_error("Expected 'y' to have total length of %d (%d x %d)",
             ny * n, ny, n);
  }
  interpolate_data * data = interpolate_alloc(type, n, ny, x, y);

  SEXP r_ptr = R_MakeExternalPtr(data, R_NilValue, R_NilValue);
  R_RegisterCFinalizer(r_ptr, interpolate_data_finalize);

  return r_ptr;
}

interpolate_data* interpolate_data_get(SEXP r_ptr, bool closed_error) {
  interpolate_data *data = NULL;
  if (TYPEOF(r_ptr) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  data = (interpolate_data*) R_ExternalPtrAddr(r_ptr);
  if (!data && closed_error) {
    Rf_error("interpolate_data already freed");
  }
  return data;
}

void interpolate_data_finalize(SEXP r_ptr) {
  interpolate_data *data = interpolate_data_get(r_ptr, false);
  if (data) {
    interpolate_free(data);
    R_ClearExternalPtr(r_ptr);
  }
}

typedef int (*interpolate_run_t)(double, interpolate_data*, double*);

SEXP interpolate_eval(SEXP r_ptr, SEXP r_x) {
  interpolate_data * obj = interpolate_data_get(r_ptr, true);
  size_t nx = (size_t) length(r_x), ny = obj->ny;
  const double *x = REAL(r_x);
  SEXP r_y = PROTECT(allocVector(REALSXP, nx * ny));
  double * y = REAL(r_y);

  interpolate_run_t run = NULL;
  switch(obj->type) {
  case CONSTANT:
    run = &interpolate_constant_run;
    break;
  case LINEAR:
    run = &interpolate_linear_run;
    break;
  case SPLINE:
    run = &interpolate_spline_run;
    break;
  }

  for (size_t i = 0; i < nx; ++i) {
    run(x[i], obj, y);
    y += ny;
  }

  UNPROTECT(1);
  return r_y;
}

#include <R_ext/Rdynload.h>
#include <Rversion.h>

static const R_CallMethodDef call_methods[] = {
  {"Cinterpolate_prepare",     (DL_FUNC) &interpolate_prepare,     3},
  {"Cinterpolate_eval",        (DL_FUNC) &interpolate_eval,        2},
  {NULL,                       NULL,                               0}
};

// Package initialisation, required for the registration
void R_init_odin(DllInfo *dll) {
  R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 3, 0)
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
#endif
}
