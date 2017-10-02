#include <R.h>
#include <Rinternals.h>

SEXP interpolate_prepare(SEXP r_x, SEXP r_y, SEXP r_type);
SEXP interpolate_eval(SEXP r_ptr, SEXP r_x);
