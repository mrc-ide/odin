double user_get_scalar_double(SEXP user, const char *name,
                              double default_value, double min, double max) {
  double ret = default_value;
  SEXP el = user_list_element(user, name);
  if (el != R_NilValue) {
    if (length(el) != 1) {
      Rf_error("Expected a scalar numeric for '%s'", name);
    }
    if (TYPEOF(el) == REALSXP) {
      ret = REAL(el)[0];
    } else if (TYPEOF(el) == INTSXP) {
      ret = INTEGER(el)[0];
    } else {
      Rf_error("Expected a numeric value for '%s'", name);
    }
  }
  if (ISNA(ret)) {
    Rf_error("Expected a value for '%s'", name);
  }
  user_check_values_double(&ret, 1, min, max, name);
  return ret;
}

int user_get_scalar_int(SEXP user, const char *name,
                        int default_value, double min, double max) {
  int ret = default_value;
  SEXP el = user_list_element(user, name);
  if (el != R_NilValue) {
    if (length(el) != 1) {
      Rf_error("Expected scalar integer for '%d'", name);
    }
    if (TYPEOF(el) == REALSXP) {
      double tmp = REAL(el)[0];
      if (fabs(tmp - round(tmp)) > 2e-8) {
        Rf_error("Expected '%s' to be integer-like", name);
      }
    }
    ret = INTEGER(coerceVector(el, INTSXP))[0];
  }
  if (ret == NA_INTEGER) {
    Rf_error("Expected a value for '%s'", name);
  }
  user_check_values_int(&ret, 1, min, max, name);
  return ret;
}

SEXP user_list_element(SEXP list, const char *name) {
  SEXP ret = R_NilValue, names = getAttrib(list, R_NamesSymbol);
  for (int i = 0; i < length(list); ++i) {
    if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0) {
      ret = VECTOR_ELT(list, i);
      break;
    }
  }
  return ret;
}

void odin_set_dim(SEXP target, int rank, ...) {
  SEXP r_dim = PROTECT(allocVector(INTSXP, rank));
  int *dim = INTEGER(r_dim);

  va_list ap;
  va_start(ap, rank);
  for (size_t i = 0; i < (size_t)rank; ++i) {
    dim[i] = va_arg(ap, int);
  }
  va_end(ap);

  setAttrib(target, R_DimSymbol, r_dim);
  UNPROTECT(1);
}

// get an array of known size
void* user_get_array(SEXP user, bool is_integer, void * previous,
                     const char *name, double min, double max,
                     int rank, ...) {
  SEXP el = user_get_array_check_rank(user, name, rank, previous == NULL);
  if (el == R_NilValue) {
    return previous;
  }

  SEXP r_dim;
  int *dim;

  size_t len = LENGTH(el);
  if (rank == 1) {
    r_dim = PROTECT(ScalarInteger(len));
  } else {
    r_dim = PROTECT(coerceVector(getAttrib(el, R_DimSymbol), INTSXP));
  }
  dim = INTEGER(r_dim);

  va_list ap;
  va_start(ap, rank);
  for (size_t i = 0; i < (size_t) rank; ++i) {
    int dim_expected = va_arg(ap, int);
    if (dim[i] != dim_expected) {
      va_end(ap); // avoid a leak
      if (rank == 1) {
        Rf_error("Expected length %d value for '%s'", dim_expected, name);
      } else {
        Rf_error("Incorrect size of dimension %d of '%s' (expected %d)",
                 i + 1, name, dim_expected);
      }
    }
  }
  va_end(ap);
  UNPROTECT(1);

  el = PROTECT(user_get_array_check(el, is_integer, name, min, max));

  void *dest = NULL;
  if (is_integer) {
    dest = R_Calloc(len, int);
    memcpy(dest, INTEGER(el), len * sizeof(int));
  } else {
    dest = R_Calloc(len, double);
    memcpy(dest, REAL(el), len * sizeof(double));
  }
  R_Free(previous);

  UNPROTECT(1);

  return dest;
}


SEXP user_get_array_check(SEXP el, bool is_integer, const char *name,
                          double min, double max) {
  size_t len = (size_t) length(el);
  if (is_integer) {
    if (TYPEOF(el) == INTSXP) {
      user_check_values_int(INTEGER(el), len, min, max, name);
    } else if (TYPEOF(el) == REALSXP) {
      el = PROTECT(coerceVector(el, INTSXP));
      user_check_values_int(INTEGER(el), len, min, max, name);
      UNPROTECT(1);
    } else {
      Rf_error("Expected a integer value for '%s'", name);
    }
  } else {
    if (TYPEOF(el) == INTSXP) {
      el = PROTECT(coerceVector(el, REALSXP));
      user_check_values_double(REAL(el), len, min, max, name);
      UNPROTECT(1);
    } else if (TYPEOF(el) == REALSXP) {
      user_check_values_double(REAL(el), len, min, max, name);
    } else {
      Rf_error("Expected a numeric value for '%s'", name);
    }
  }
  return el;
}

SEXP user_get_array_check_rank(SEXP user, const char *name, int rank,
                               bool required) {
  SEXP el = user_list_element(user, name);
  if (el == R_NilValue) {
    if (required) {
      Rf_error("Expected a value for '%s'", name);
    }
  } else {
    if (rank == 1) {
      if (isArray(el)) {
        Rf_error("Expected a numeric vector for '%s'", name);
      }
    } else {
      SEXP r_dim = getAttrib(el, R_DimSymbol);
      if (r_dim == R_NilValue || LENGTH(r_dim) != rank) {
        if (rank == 2) {
          Rf_error("Expected a numeric matrix for '%s'", name);
        } else {
          Rf_error("Expected a numeric array of rank %d for '%s'", rank, name);
        }
      }
    }
  }
  return el;
}

void* user_get_array_dim(SEXP user, bool is_integer, void * previous,
                         const char *name, int rank,
                         double min, double max, int *dest_dim) {
  SEXP el = user_get_array_check_rank(user, name, rank, previous == NULL);
  if (el == R_NilValue) {
    return previous;
  }

  dest_dim[0] = LENGTH(el);
  if (rank > 1) {
    SEXP r_dim = PROTECT(coerceVector(getAttrib(el, R_DimSymbol), INTSXP));
    int *dim = INTEGER(r_dim);

    for (size_t i = 0; i < (size_t) rank; ++i) {
      dest_dim[i + 1] = dim[i];
    }

    UNPROTECT(1);
  }

  el = PROTECT(user_get_array_check(el, is_integer, name, min, max));

  int len = LENGTH(el);
  void *dest = NULL;
  if (is_integer) {
    dest = R_Calloc(len, int);
    memcpy(dest, INTEGER(el), len * sizeof(int));
  } else {
    dest = R_Calloc(len, double);
    memcpy(dest, REAL(el), len * sizeof(double));
  }
  R_Free(previous);

  UNPROTECT(1);

  return dest;
}


void user_check_values(SEXP value, double min, double max,
                           const char *name) {
  size_t len = (size_t)length(value);
  if (TYPEOF(value) == INTSXP) {
    user_check_values_int(INTEGER(value), len, min, max, name);
  } else {
    user_check_values_double(REAL(value), len, min, max, name);
  }
}


void user_check_values_int(int * value, size_t len,
                               double min, double max, const char *name) {
  for (size_t i = 0; i < len; ++i) {
    if (ISNA(value[i])) {
      Rf_error("'%s' must not contain any NA values", name);
    }
  }
  if (min != NA_REAL) {
    for (size_t i = 0; i < len; ++i) {
      if (value[i] < min) {
        Rf_error("Expected '%s' to be at least %g", name, min);
      }
    }
  }
  if (max != NA_REAL) {
    for (size_t i = 0; i < len; ++i) {
      if (value[i] > max) {
        Rf_error("Expected '%s' to be at most %g", name, max);
      }
    }
  }
}


void user_check_values_double(double * value, size_t len,
                                  double min, double max, const char *name) {
  for (size_t i = 0; i < len; ++i) {
    if (ISNA(value[i])) {
      Rf_error("'%s' must not contain any NA values", name);
    }
  }
  if (min != NA_REAL) {
    for (size_t i = 0; i < len; ++i) {
      if (value[i] < min) {
        Rf_error("Expected '%s' to be at least %g", name, min);
      }
    }
  }
  if (max != NA_REAL) {
    for (size_t i = 0; i < len; ++i) {
      if (value[i] > max) {
        Rf_error("Expected '%s' to be at most %g", name, max);
      }
    }
  }
}


// modulo that conforms to (approximately) the same behaviour as R
double fmodr(double x, double y) {
  double tmp = fmod(x, y);
  if (tmp * y < 0) {
    tmp += y;
  }
  return tmp;
}

// this probably does not need to be done separately (could be
// inlined) but we'll let the compiler do that for us.  Keeping it out
// means if I find out that it's really platform dependent we can
// tweak that here.
double fintdiv(double x, double y) {
  return floor(x / y);
}


double odin_sum1(double *x, size_t from, size_t to) {
  double tot = 0.0;
  for (size_t i = from; i < to; ++i) {
    tot += x[i];
  }
  return tot;
}

int odin_isum1(int *x, size_t from, size_t to) {
  int tot = 0.0;
  for (size_t i = from; i < to; ++i) {
    tot += x[i];
  }
  return tot;
}

void lagvalue_ds(double t, int *idx, int dim_idx, double *state) {
  typedef void (*lagvalue_type)(double, int*, int, double*);
  static lagvalue_type fun = NULL;
  if (fun == NULL) {
    fun = (lagvalue_type)R_GetCCallable("deSolve", "lagvalue");
  }
  fun(t, idx, dim_idx, state);
}

void lagvalue_dde(double t, int *idx, size_t dim_idx, double *state) {
  typedef void (*lagvalue_type)(double, int*, size_t, double*);
  static lagvalue_type fun = NULL;
  if (fun == NULL) {
    fun = (lagvalue_type)R_GetCCallable("dde", "ylag_vec_int");
  }
  fun(t, idx, dim_idx, state);
}


void lagvalue(double t, bool use_dde, int *idx, int dim_idx, double *state) {
  if (use_dde) {
    lagvalue_dde(t, idx, dim_idx, state);
  } else {
    lagvalue_ds(t, idx, dim_idx, state);
  }
}

// check here, given information on the type, that we have at least 1
// point for type 0, 2 for type 1, and 3 for type 2.  That should work
// pretty happily.
void interpolate_check_y(size_t nx, size_t ny, size_t i, const char *name_arg, const char *name_target) {
  if (nx != ny) {
    if (i == 0) {
      // vector case
      Rf_error("Expected %s to have length %d (for '%s')",
               name_arg, nx, name_target);
    } else {
      // array case
      Rf_error("Expected dimension %d of %s to have size %d (for '%s')",
               i, name_arg, nx, name_target);
    }
  }
}


void rmhyper(size_t n_sample, int *k, size_t m) {
  int N = 0;
  for (size_t i = 0; i < m; ++i) {
    N += k[i];
  }
  if ((int)n_sample > N) {
    Rf_error("Requesting too many elements in rmhyper (%d from %d)",
             n_sample, N);
  }
  int k_other = N - k[0];
  k[0] = Rf_rhyper(k[0], k_other, n_sample);
  for (size_t i = 1; i < m - 1; ++i) {
    k_other -= k[i];
    n_sample -= k[i - 1];
    k[i] = Rf_rhyper(k[i], k_other, n_sample);
  }
  k[m - 1] = n_sample - k[m - 2];
}


void rmhyper_i(size_t n_sample, int *k, size_t m, int *ret) {
  memcpy(ret, k, m * sizeof(int));
  rmhyper(n_sample, ret, m);
}


void rmhyper_d(size_t n_sample, double *k, size_t m, int *ret) {
  for (size_t i = 0; i < m; ++i) {
    ret[i] = k[i];
  }
  rmhyper(n_sample, ret, m);
}


double scalar_real(SEXP x, const char * name) {
  if (Rf_length(x) != 1) {
    Rf_error("Expected a scalar for '%s'", name);
  }
  double ret = 0.0;
  if (TYPEOF(x) == INTSXP) {
    ret = INTEGER(x)[0];
  } else if (TYPEOF(x) == REALSXP) {
    ret = REAL(x)[0];
  } else {
    Rf_error("Expected a numeric value for '%s'", name);
  }
  return ret;
}


int scalar_int(SEXP x, const char * name) {
  if (Rf_length(x) != 1) {
    Rf_error("Expected a scalar for '%s'", name);
  }
  int ret = 0;
  if (TYPEOF(x) == INTSXP) {
    ret = INTEGER(x)[0];
  } else if (TYPEOF(x) == REALSXP) {
    double rx = REAL(x)[0];
    ret = rx;
    if (fabs(rx - ret) > sqrt(DBL_EPSILON)) {
      Rf_error("Expected a integer-like for '%s'", name);
    }
  } else {
    Rf_error("Expected an integer value for '%s'", name);
  }
  return ret;
}
