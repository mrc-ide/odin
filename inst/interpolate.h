#include <stddef.h>

typedef enum {
  CONSTANT,
  LINEAR,
  SPLINE
} interpolate_type;

typedef struct interpolate_data {
  interpolate_type type;
  size_t n;     // number of points
  size_t ny;    // number of y entries per 'x' point
  size_t i;     // index of last point checked
  double *x;    // x points of interpolation
  double *y;    // y points of interpolation
  double *k;    // knots when using spline interpolation
} interpolate_data;

interpolate_data * interpolate_alloc(interpolate_type type, size_t n,
                                     size_t ny, double *x, double *y);
void interpolate_free(interpolate_data* obj);
int interpolate_constant_run(double x, interpolate_data* obj, double *y);
int interpolate_linear_run(double x, interpolate_data* obj, double *y);
int interpolate_spline_run(double x, interpolate_data* obj, double *y);

// Utility:
int interpolate_search(double target, interpolate_data *obj);

// Splines
double spline_eval_i(size_t i, double x, double *xs, double *ys, double *ks);

void spline_calc_A(size_t n, double *x, double *A);
void spline_calc_B(size_t n, size_t ny, double *x, double *y, double *B);
void spline_calc_solve(int n, int ny, double *A, double *B);

// Interface:
void odin_interpolate_check(size_t nx, size_t ny, size_t i, const char *name_arg, const char *name_target);
