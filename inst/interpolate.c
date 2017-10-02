#include "interpolate.h"
#include <R_ext/Lapack.h>
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
interpolate_data * interpolate_alloc(interpolate_type type,
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

  if (type == SPLINE) {
    ret->k = (double*) Calloc(n * ny, double);
    double *A = (double*)R_alloc(n * 3, sizeof(double));
    spline_calc_A(n, x, A);
    spline_calc_B(n, ny, x, y, ret->k);
    spline_calc_solve(n, ny, A, ret->k);
  }
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

// int interpolate_run(double x, interpolate_data *obj, double *y) {
// do a switch here...
//}

// Constant
int interpolate_constant_run(double x, interpolate_data *obj, double *y) {
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
  // TODO: In general, I wonder if this should be dealt with in interpolate
  // search?
  //
  // NOTE: In the R function 'approx' there is an argument 'f' that
  // deals with the 'ties' case more gracefully.  This is like the
  // default f=0, omitting this becomes like the option f=1.
  if (i != (int)obj->n - 1 && obj->x[i + 1] == x) {
    ++i;
  }

  double *y0 = obj->y + i;
  for (size_t j = 0; j < obj->ny; ++j, y0 += obj->n) {
    y[j] = *y0;
  }
  return 0;
}

// Linear
int interpolate_linear_run(double x, interpolate_data* obj, double *y) {
  int i = interpolate_search(x, obj);
  // In theory we might be able to handle this, but it's simpler to
  // forbid it I think.  In odin we'll do a check that the
  // interpolation times span the entire range of integration times.
  if (i < 0 || i == (int)obj->n) { // off the lhs or rhs
    for (size_t j = 0; j < obj->ny; ++j) {
      y[j] = NA_REAL;
    }
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

// Spline
int interpolate_spline_run(double x, interpolate_data* obj, double *y) {
  int i = interpolate_search(x, obj);
  if (i < 0 || i == (int)obj->n) { // off the lhs or rhs
    for (size_t j = 0; j < obj->ny; ++j) {
      y[j] = NA_REAL;
    }
    return -1;
  }
  double *ys = obj->y, *ks = obj->k;
  for (size_t j = 0; j < obj->ny; ++j, ys += obj->n, ks += obj->n) {
    y[j] = spline_eval_i(i, x, obj->x, ys, ks);
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
    i1 = i0 + inc;
    while (x[i1] < target) {
      i0 = i1;
      inc *= 2;
      i1 += inc;
      if (i1 >= (int)n) { // off the end of the buffer
        i1 = n - 1;
        if (x[i1] < target) {
          return n;
        }
        break;
      }
    }
  } else { // advance down
    if (i0 == 0) { // guess is already at the bottom
      return -1;
    }
    i0 = i0 - inc;
    while (x[i0] > target) {
      i1 = i0;
      inc *= 2;
      if (i0 < inc) {
        i0 = 0;
        if (x[i0] > target) {
          return -1;
        }
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

double spline_eval_i(size_t i, double x, double *xs, double *ys, double *ks) {
  double t = (x - xs[i]) / (xs[i + 1] - xs[i]);
  double a =  ks[i] * (xs[i + 1] - xs[i]) - (ys[i + 1] - ys[i]);
  double b = -ks[i + 1] * (xs[i + 1] - xs[i]) + (ys[i + 1] - ys[i]);
  return (1 - t) * ys[i] + t * ys[i + 1] + t * (1 - t) * (a * (1 - t) + b * t);
}

void spline_calc_A(size_t n, double *x, double *A) {
  double *A0 = A, *A1 = A + n, *A2 = A + 2 * n;
  size_t nm1 = n - 1;

  A0[0] = 0; // will be ignored
  A1[0] = 2 / (x[1] - x[0]);
  A2[0] = 1 / (x[1] - x[0]);
  for (size_t i = 1; i < nm1; ++i) {
    A0[i] = 1 / (x[i] - x[i - 1]);
    A1[i] = 2 * (1 / (x[i] - x[i - 1]) + 1 / (x[i + 1] - x[i]));
    A2[i] = 1 / (x[i + 1] - x[i]);
  }
  A0[nm1] = 1 / (x[nm1] - x[nm1-1]);
  A1[nm1] = 2 / (x[nm1] - x[nm1-1]);
  A2[nm1] = 0; // will be ignored
}

void spline_calc_B(size_t n, size_t ny, double *x, double *y, double *B) {
  size_t nm1 = n - 1;
  for (size_t i = 0; i < ny; ++i) {
    B[0] = 3 * (y[1] - y[0]) / ((x[1] - x[0]) * (x[1] - x[0]));
    for (size_t i = 1; i < nm1; ++i) {
      B[i] = 3 *
        ((y[i]   - y[i-1]) / ((x[i  ] - x[i-1]) * (x[i  ] - x[i-1])) +
         (y[i+1] - y[i  ]) / ((x[i+1] - x[i  ]) * (x[i+1] - x[i  ])));
    }
    B[nm1] = 3 *
      (y[nm1] - y[nm1-1]) / ((x[nm1] - x[nm1-1]) * (x[nm1] - x[nm1-1]));
    B += n;
  }
}

void spline_calc_solve(int n, int ny, double *A, double *B) {
  int info = 0, ldb = n;
  double *du = A + 1, *d = A + n, *dl = A + n * 2;
  F77_NAME(dgtsv)(&n, &ny, dl, d, du, B, &ldb, &info);
}

// check here, given information on the type, that we have at least 1
// point for type 0, 2 for type 1, and 3 for type 2.  That should work
// pretty happily.
void odin_interpolate_check(size_t nx, size_t ny, size_t i, const char *name_arg, const char *name_target) {
  if (nx != ny) {
    if (i == 0) {
      // vector case
      Rf_error("Expected %s to have length %d (for %s)",
               name_arg, nx, name_target);
    } else {
      // array case
      Rf_error("Expected dimension %d of %s to have size %d (for %s)",
               i, name_arg, nx, name_target);
    }
  }
}
