#include "interpolate.h"
#include <R.h> // memory and error management

// TODO: Do we need to copy here?  If I use this from R the copy will
// be needed because otherwise the pointers need care.  But for the
// purposes of odin, we could use the same memory.  It might be worth
// a copy flag here, but that's only a memory optimisation not a CPU
// one.  Hold off dealing with any of this until the spline bits are
// done here; they're a bit nastier, but should not be all that
// terrible to get right -- but because they take more space it's not
// clear that there will be a bit advantage in not copying here
// (storing the knots etc).
interpolate_data * interpolate_alloc(size_t type,
                                     size_t n, size_t ny,
                                     double *x, double *y) {
  interpolate_data * ret = Calloc(1, interpolate_data);
  ret->type = type;
  ret->n = n;
  ret->ny = ny;
  ret->i = 0;
  ret->x = (double*) Calloc(n, double);
  ret->y = (double*) Calloc(n * ny, double);
  ret->k = NULL;
  memcpy(ret->x, x, sizeof(double) * n);
  memcpy(ret->y, y, sizeof(double) * n * ny);
  return ret;
}

void interpolate_free(interpolate_data* obj) {
  if (obj) {
    Free(obj->x);
    Free(obj->y);
    Free(obj->k);
    Free(obj);
  }
}

//int interpolate_run(double x, interpolate_data *obj, double *y) {
//  // do a switch here...
//}

int interpolate_0_run(double x, interpolate_data *obj, double *y) {
  // Do a hunt/bisect search here
  int i = interpolate_search(x, obj);
  // In theory we might be able to handle this, but it's simpler to
  // forbid it I think.  In odin we'll do a check that the
  // interpolation times span the entire range of integration times.
  if (i < 0) {
    for (size_t j = 0; j < obj->ny; ++j) {
      y[j] = NA_REAL;
    }
    return -1;
  } else if (i == (int) obj->n) { // off the rhs
    i = obj->n - 1;
  }

  double *y0 = obj->y + i;
  for (size_t j = 0; j < obj->ny; ++j, y0 += obj->n) {
    y[j] = *y0;
  }
  return 0;
}

int interpolate_1_run(double x, interpolate_data* obj, double *y) {
  int i = interpolate_search(x, obj);
  // In theory we might be able to handle this, but it's simpler to
  // forbid it I think.  In odin we'll do a check that the
  // interpolation times span the entire range of integration times.
  if (i < 0 || i == (int)obj->n) { // off the lhs or rhs
    y[0] = NA_REAL;
    return -1;
  }

  // Here we need to compute some things.
  //
  // TODO: deal with the case where i+1 is out of bounds; in that
  // case, it must be the case that x0 is equal to x.  This affects y1
  // in the same way.
  double x0 = obj->x[i], x1 = obj->x[i + 1];
  double scal = (x - x0) / (x1 - x0);

  double *y0 = obj->y + i;
  double *y1 = y0 + 1;

  for (size_t j = 0; j < obj->ny; ++j, y0 += obj->n, y1 += obj->n) {
    y[j] = *y0 + (*y1 - *y0) * scal;
  }

  return 0;
}

int interpolate_search(double target, interpolate_data *obj) {
  int i0 = (int)obj->i, i1 = (int)obj->i, inc = 1;
  size_t n = obj->n;
  double *x = obj->x;

  if (x[i0] <= target) { // advance up until we hit the top
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
    while (x[i0] > target) {
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
    obj->i = (size_t)i1;
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

  obj->i = (size_t)i0;
  return i0;
}
