// constant and linear support
typedef struct interpolate_0_data {
  size_t n;    // number of points
  size_t ny;   // number of y entries per 'x' point
  size_t i;    // index of last point checked
  double *x;   // x points of interpolation
  double *y;   // y points of interpolation
} interpolate_0_data;
