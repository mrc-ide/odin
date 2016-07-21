#include "interpolate.h"
#include <R.h> // memory and error management

// TODO: Do we need to copy here?  If I use this from R the copy will
// be needed because otherwise the pointers need care.  But for the
// purposes of odin, we could use the same memory.  It might be worth
// a copy flag here, but that's only a memory optimisation not a CPU
// one.
interpolate_0_data * interpolate_0_data_alloc(size_t n, size_t ny,
                                              double *x, double *y) {
  interpolate_0_data * ret = Calloc(1, interpolate_0_data);
  ret->n = n;
  ret->ny = ny;
  ret->i = 0;
  ret->x = (double*) Calloc(n, double);
  ret->y = (double*) Calloc(n * ny, double);
  memcpy(ret->x, x, sizeof(double) * n);
  memcpy(ret->y, y, sizeof(double) * n * ny);
  return ret;
}

void interpolate_0_data_free(interpolate_0_data* obj) {
  Free(obj->x);
  Free(obj->y);
  Free(obj);
}

int interpolate_0_data_run(double x, interpolate_0_data* obj, double *y) {
  // Do a hunt/bisect search here
  // TODO: starting from 0 here this is a bit broken at the moment (see test)
  int i = search(x, 0, obj->n, obj->x);
  // In theory we might be able to handle this, but it's simpler to
  // forbid it I think.  In odin we'll do a check that the
  // interpolation times span the entire range of integration times.
  if (i < 0 || i == (int)obj->n) { // off the lhs or rhs
    y[0] = NA_REAL;
    return -1;
  }
  obj->i = i; // save last lookup
  double *y0 = obj->y + i * obj->ny;
  memcpy(y, y0, obj->ny * sizeof(double));
  return 0;
}

int interpolate_1_data_run(double x, interpolate_0_data* obj, double *y) {
  int i = search(x, 0, obj->n, obj->x);
  // In theory we might be able to handle this, but it's simpler to
  // forbid it I think.  In odin we'll do a check that the
  // interpolation times span the entire range of integration times.
  if (i < 0 || i == (int)obj->n) { // off the lhs or rhs
    y[0] = NA_REAL;
    return -1;
  }
  obj->i = i; // save last lookup

  // Here we need to compute some things.
  //
  // TODO: deal with the case where i+1 is out of bounds; in that
  // case, it must be the case that x0 is equal to x.  This affects y1
  // in the same way.
  double x0 = obj->x[i], x1 = obj->x[i + 1];
  double scal = (x - x0) / (x1 - x0);

  double *y0 = obj->y + i * obj->ny;
  double *y1 = y0 + obj->ny;

  for (size_t j = 0; j < obj->ny; ++j) {
    y[j] = y0[j] + (y1[j] - y0[j]) * scal;
  }

  return 0;
}

int search(double target, size_t i, size_t n, double *x) {
  int i0 = (int)i, i1 = (int)i, inc = 1;
  if (x[i] < target) { // advance up until we hit the top
    if (i0 == (int)n - 1) { // guess is already *at* the top.
      return n;
    }
    i1 = i0 + 1;
    while (x[i1] < target) {
      i0 = i1;
      inc *= 2;
      i1 += inc;
      if (i1 >= (int)n) { // off the end of the buffer
        i1 = n - 1;
        break;
      }
    }
  } else { // advance down
    if (i0 == 0) { // guess is already at the bottom
      return -1;
    }
    i0 = i0 - 1;
    while (x[i1] > target) {
      i1 = i0;
      inc *= 2;
      if (i0 < inc) {
        i0 = 0;
        break;
      }
      i0 -= inc;
    }
  }

  // Need to deal specially with this case apparently, but not sure
  // why.  It's possible that this only needs doing on one of the
  // early exits from the above loops.
  if (i1 - i0 == 1 && x[i1] < target) {
    return i1;
  }

  while (i1 - i0 > 1) {
    int i2 = (i1 + i0) / 2;
    if (x[i2] < target) {
      i0 = i2;
    } else {
      i1 = i2;
    }
  }

  return i0;
}
