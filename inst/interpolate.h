#ifndef _ODIN_INTERPOLATE_H_
#define _ODIN_INTERPOLATE_H_

#include <stddef.h>

// constant (linear will be the same, actually)
typedef struct interpolate_0_data {
  size_t n;    // number of points
  size_t ny;   // number of y entries per 'x' point
  size_t i;    // index of last point checked
  double *x;   // x points of interpolation
  double *y;   // y points of interpolation
} interpolate_0_data;

interpolate_0_data * interpolate_0_data_alloc(size_t n, size_t ny,
                                              double *x, double *y);
void interpolate_0_data_free(interpolate_0_data* obj);
int interpolate_0_data_run(double x, interpolate_0_data* obj, double *y);
int interpolate_1_data_run(double x, interpolate_0_data* obj, double *y);

// Utility:
int search(double target, size_t i, size_t n, double *x);

#endif
