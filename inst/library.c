SEXP get_ds_pars() {
  static DL_FUNC get_deSolve_gparms = NULL;
  if (get_deSolve_gparms == NULL) {
    get_deSolve_gparms = R_GetCCallable("deSolve", "get_deSolve_gparms");
  }
  return get_deSolve_gparms();
}

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
    Rf_error("Expected value for %s", name);
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
    Rf_error("Expected value for %s", name);
  }
  return ret;
}

void get_user_vec(SEXP user, const char *name, int len, double *ret) {
  SEXP el = get_list_element(user, name);
  if (el != R_NilValue) {
    if (length(el) != len) {
      Rf_error("Incorrect length for %s; expected %d received %d",
               name, len, length(el));
    }
    el = PROTECT(coerceVector(el, REALSXP));
    memcpy(ret, REAL(el), len * sizeof(double));
    UNPROTECT(1);
  }
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

void lagvalue_discrete(int step, int *idx, size_t dim_idx, double *state) {
  typedef void (*lagvalue_type)(int, int*, size_t, double*);
  static lagvalue_type fun = NULL;
  if (fun == NULL) {
    fun = (lagvalue_type)R_GetCCallable("dde", "yprev_vec_int");
  }
  fun(step, idx, dim_idx, state);
}

// Variadic functions are not the most lovely, but this avoids the big
// hassle of generating an array, passing it through and copying
void odin_set_dim(SEXP target, int nd, ...) {
  SEXP r_dim = PROTECT(allocVector(INTSXP, nd));
  int *dim = INTEGER(r_dim);

  va_list ap;
  va_start(ap, nd);
  for (size_t i = 0; i < (size_t)nd; ++i) {
    dim[i] = va_arg(ap, int);
  }
  va_end(ap);

  setAttrib(target, R_DimSymbol, r_dim);
  UNPROTECT(1);
}

// The ScalarInteger bit below feels unnecessarily rubbish, but avoids
// duplicating the variadic arg loop at the cost of a slightly odd R
// object creation.
//
// TODO: can I get nd here to be size_t? (and elsewhere below)
void get_user_array(SEXP user, const char *name, bool is_real, void *dest, int nd, ...) {
  SEXP el = get_user_array_check_rank(user, name, nd);
  SEXP r_dim;
  int *dim;

  if (nd == 1) {
    r_dim = PROTECT(ScalarInteger(LENGTH(el)));
  } else {
    r_dim = PROTECT(coerceVector(getAttrib(el, R_DimSymbol), INTSXP));
  }
  dim = INTEGER(r_dim);

  va_list ap;
  va_start(ap, nd);
  for (size_t i = 0; i < (size_t) nd; ++i) {
    int dim_expected = va_arg(ap, int);
    if (dim[i] != dim_expected) {
      va_end(ap); // avoid a leak
      if (nd == 1) {
        Rf_error("Expected length %d value for %s", dim_expected, name);
      } else {
        Rf_error("Incorrect size of dimension %d of %s (expected %d)",
                 i + 1, name, dim_expected);
      }
    }
  }
  va_end(ap);
  UNPROTECT(1);

  get_user_array_copy(el, name, is_real, dest);
}

double* get_user_array_dim(SEXP user, const char *name, bool is_real, int nd, int *dest_dim) {
  SEXP el = get_user_array_check_rank(user, name, nd);

  if (nd == 1) {
    dest_dim[0] = LENGTH(el);
  } else {
    SEXP r_dim = PROTECT(coerceVector(getAttrib(el, R_DimSymbol), INTSXP));
    int *dim = INTEGER(r_dim);

    for (size_t i = 0; i < (size_t) nd; ++i) {
      dest_dim[i] = dim[i];
    }

    UNPROTECT(1);
  }

  void *dest;
  if (is_real) {
    dest = (double*) Calloc(length(el), double);
  } else {
    dest = (int*) Calloc(length(el), int);
  }
  get_user_array_copy(el, name, is_real, dest);
  return dest;
}

SEXP get_user_array_check_rank(SEXP user, const char *name, int nd) {
  SEXP el = get_list_element(user, name);
  if (el == R_NilValue) {
    Rf_error("Expected value for %s", name);
  } else {
    if (nd == 1) {
      if (isArray(el)) {
        // this may be too strict as a length-1 dim here will fail
        Rf_error("Expected a vector for %s", name);
      }
    } else {
      SEXP r_dim = getAttrib(el, R_DimSymbol);
      if (r_dim == R_NilValue || LENGTH(r_dim) != nd) {
        if (nd == 2) {
          Rf_error("Expected a matrix for %s", name);
        } else {
          Rf_error("Expected a %dd array for %s", nd, name);
        }
      }
    }
  }
  return el;
}

void get_user_array_copy(SEXP el, const char *name, bool is_real, void *dest) {
  int given_int = TYPEOF(el) == INTSXP;
  size_t n = (size_t) length(el);
  if (is_real) {
    if (given_int) {
      el = PROTECT(coerceVector(el, REALSXP));
    } else if (TYPEOF(el) != REALSXP) {
      Rf_error("Expected a numeric value for %s", name);
    }
    memcpy(dest, REAL(el), n * sizeof(double));
  } else {
    if (TYPEOF(el) == REALSXP) {
      el = PROTECT(coerceVector(el, INTSXP));
    } else if (TYPEOF(el) != INTSXP) {
      Rf_error("Expected a numeric value for %s", name);
    }
    memcpy(dest, INTEGER(el), n * sizeof(int));
  }
  if (given_int == is_real) {
    UNPROTECT(1);
  }
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

double odin_array_at1(double *x, int i, size_t len,
                      const char * variable, const char * expr_str) {
  if (i < 0 || i >= (int)len) {
    Rf_error("Array index %d is out of bounds [1, %d] while reading %s in\n%s",
             i + 1, len, variable, expr_str);
  }
  return x[i];
}

void odin_array_at_set1(double *x, int i, double value, size_t len,
                        const char * variable, const char * expr_str) {
  if (i < 0 || i >= (int)len) {
    Rf_error("Array index %d is out of bounds [1, %d] while setting %s in\n%s",
             i + 1, len, variable, expr_str);
  }
  x[i] = value;
}

void odin_array_check(size_t nd, int *i, size_t *len,
                      const char * variable, const char * expr_str,
                      bool set) {
  bool ok[8]; // 8 is odin max dimensions
  bool pass = true;
  for (size_t d = 0; d < nd; ++d) {
    ok[d] = i[d] >= 0 && i[d] < (int)len[d];
    pass &= ok[d];
  }

  if (!pass) {
    // What we need to do here is compute the string length.  First
    // all the bits that vary:
    size_t w = i[0];
    for (size_t d = 1; d < nd; ++d) {
      w += 2 * floor(log10(len[d]));
    }
    w += 80 + // generous length of error string
      45 * nd + // per dimension message length
      strlen(expr_str) + //
      strlen(variable) * nd;
    char * msg = (char*) R_alloc(w, sizeof(char));
    memset(msg, 0, w);
    const char * action = set ? "setting" : "reading";
    size_t loc = sprintf(msg, "Array index %s[%d", variable, i[0] + 1);
    for (size_t d = 1; d < nd; ++d) {
      loc += sprintf(msg + loc, ", %d", i[d] + 1);
    }
    loc += sprintf(msg + loc, "] is out of bounds while %s in\n%s",
                   action, expr_str);
    for (size_t d = 0; d < nd; ++d) {
      if (!ok[d]) {
        loc += sprintf(msg + loc,
                       "\n - dimension %d: index %d is not in [1, %d]",
                       (int)d + 1, i[d] + 1, (int)len[d]);
      }
    }
    Rf_error(msg);
  }
}
