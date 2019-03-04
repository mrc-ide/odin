double get_user_double(SEXP user, const char *name, double default_value) {
  double ret = default_value;
  SEXP el = get_list_element(user, name);
  if (el != R_NilValue) {
    if (length(el) != 1) {
      Rf_error("Expected scalar numeric for %s", name);
    }
    if (TYPEOF(el) == REALSXP) {
      ret = REAL(el)[0];
    } else if (TYPEOF(el) == INTSXP) {
      ret = INTEGER(el)[0];
    } else {
      Rf_error("Expected a numeric value for %s", name);
    }
  }
  if (ISNA(ret)) {
    Rf_error("Expected a value for '%s'", name);
  }
  return ret;
}

int get_user_int(SEXP user, const char *name, int default_value) {
  int ret = default_value;
  SEXP el = get_list_element(user, name);
  if (el != R_NilValue) {
    if (length(el) != 1) {
      Rf_error("Expected scalar integer for %d", name);
    }
    ret = INTEGER(coerceVector(el, INTSXP))[0];
  }
  if (ret == NA_INTEGER) {
    Rf_error("Expected a value for '%s'", name);
  }
  return ret;
}

SEXP get_list_element(SEXP list, const char *name) {
  SEXP ret = R_NilValue, names = getAttrib(list, R_NamesSymbol);
  for (int i = 0; i < length(list); ++i) {
    if(strcmp(CHAR(STRING_ELT(names, i)), name) == 0) {
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
double* get_user_array_double(SEXP user, const char *name, int rank, ...) {
  SEXP el = get_user_array_check_rank(user, name, rank);
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
        Rf_error("Expected length %d value for %s", dim_expected, name);
      } else {
        Rf_error("Incorrect size of dimension %d of %s (expected %d)",
                 i + 1, name, dim_expected);
      }
    }
  }
  va_end(ap);
  UNPROTECT(1);

  double *dest = Calloc(len, double);
  get_user_array_copy_double(el, name, dest);
  return dest;
}

void get_user_array_copy_double(SEXP el, const char *name, void *dest) {
  size_t len = (size_t) length(el);
  if (TYPEOF(el) == INTSXP) {
    el = PROTECT(coerceVector(el, REALSXP));
    memcpy(dest, REAL(el), len * sizeof(double));
    UNPROTECT(1);
  } else if (TYPEOF(el) == REALSXP) {
    memcpy(dest, REAL(el), len * sizeof(double));
  } else {
    Rf_error("Expected a numeric value for %s", name);
  }
}

SEXP get_user_array_check_rank(SEXP user, const char *name, int rank) {
  SEXP el = get_list_element(user, name);
  if (el == R_NilValue) {
    Rf_error("Expected value for %s", name);
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

double* get_user_array_dim_double(SEXP user, const char *name, int rank, int *dest_dim) {
  SEXP el = get_user_array_check_rank(user, name, rank);

  dest_dim[0] = LENGTH(el);
  if (rank > 1) {
    SEXP r_dim = PROTECT(coerceVector(getAttrib(el, R_DimSymbol), INTSXP));
    int *dim = INTEGER(r_dim);

    for (size_t i = 0; i < (size_t) rank; ++i) {
      dest_dim[i + 1] = dim[i];
    }

    UNPROTECT(1);
  }

  double *dest = (double*) Calloc(length(el), double);
  get_user_array_copy_double(el, name, dest);
  return dest;
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
