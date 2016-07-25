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
      Rf_error("Expected scalar numeric for %d", name);
    }
    ret = REAL(coerceVector(el, REALSXP))[0];
  }
  if (ISNA(ret)) {
    Rf_error("Expected value for %s", name);
  }
  return ret;
}

void get_user_array1(SEXP user, const char *name, int len, double *dest) {
  SEXP el = get_list_element(user, name);
  if (el == R_NilValue) {
    Rf_error("Expected value for %s", name);
  } else {
    if (LENGTH(el) != len) {
      Rf_error("Expected length %d value for %s", len, name);
    }
    el = PROTECT(coerceVector(el, REALSXP));
    memcpy(dest, REAL(el), len * sizeof(double));
    UNPROTECT(1);
  }
}

// NOTE: different versions will be needed with variable size arrays,
// because we'll need to return the memory too.
void get_user_array2(SEXP user, const char *name, int nr, int nc, double *dest) {
  SEXP el = get_list_element(user, name);
  if (el == R_NilValue) {
    Rf_error("Expected value for %s", name);
  } else {
    SEXP dim = getAttrib(el, R_DimSymbol);
    if (dim == R_NilValue || LENGTH(dim) != 2) {
      Rf_error("Expected a matrix for %s", name);
    }
    dim = PROTECT(coerceVector(dim, INTSXP));
    if (INTEGER(dim)[0] != nr || INTEGER(dim)[1] != nc) {
      Rf_error("Expected a matrix of dimensions %d x %d for %s", nr, nc, name);
    }
    el = PROTECT(coerceVector(el, REALSXP));
    memcpy(dest, REAL(el), nr * nc * sizeof(double));
    UNPROTECT(2);
  }
}

void get_user_array3(SEXP user, const char *name, int nr, int nc, int nz, double *dest) {
  SEXP el = get_list_element(user, name);
  if (el == R_NilValue) {
    Rf_error("Expected value for %s", name);
  } else {
    SEXP dim = getAttrib(el, R_DimSymbol);
    if (dim == R_NilValue || LENGTH(dim) != 3) {
      Rf_error("Expected a 3d array for %s", name);
    }
    dim = PROTECT(coerceVector(dim, INTSXP));
    if (INTEGER(dim)[0] != nr ||
        INTEGER(dim)[1] != nc ||
        INTEGER(dim)[2] != nz) {
      Rf_error("Expected an array of dimensions [%d, %d, %d] for %s",
               nr, nc, nz, name);
    }
    el = PROTECT(coerceVector(el, REALSXP));
    memcpy(dest, REAL(el), nr * nc * nz * sizeof(double));
    UNPROTECT(2);
  }
}

double* get_user_array_dim1(SEXP user, const char *name, int *len) {
  SEXP el = get_list_element(user, name);
  if (el == R_NilValue) {
    Rf_error("Expected value for %s", name);
  } else if (isArray(el)) {
    // this may be too strict as a length-1 dim here will fail
    Rf_error("Expected a vector for %s", name);
  } else if (TYPEOF(el) != REALSXP) {
    Rf_error("Expected numeric vector for %s", name);
  } else {
    *len = LENGTH(el);
  }
  return REAL(el);
}

double* get_user_array_dim2(SEXP user, const char *name, int *nr, int *nc) {
  SEXP el = get_list_element(user, name);
  if (el == R_NilValue) {
    Rf_error("Expected value for %s", name);
  } else if (!isMatrix(el)) {
    // this may be too strict as a length-1 dim here will fail
    Rf_error("Expected a matrix for %s", name);
  } else if (TYPEOF(el) != REALSXP) {
    Rf_error("Expected numeric matrix for %s", name);
  } else {
    int *dim = INTEGER(getAttrib(el, R_DimSymbol));
    *nr = dim[0];
    *nc = dim[1];
  }
  return REAL(el);
}

double* get_user_array_dim3(SEXP user, const char *name, int *nr, int *nc, int *nz) {
  SEXP el = get_list_element(user, name);
  if (el == R_NilValue) {
    Rf_error("Expected value for %s", name);
  } else {
    SEXP dim = getAttrib(el, R_DimSymbol);
    if (dim == R_NilValue || LENGTH(dim) != 3) {
      Rf_error("Expected a 3d array for %s", name);
    }
    if (TYPEOF(el) != REALSXP) {
      Rf_error("Expected numeric 3d array for %s", name);
    }
    *nr = INTEGER(dim)[0];
    *nc = INTEGER(dim)[1];
    *nz = INTEGER(dim)[2];
  }
  return REAL(el);
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

// NOTE: all sums are over _inclusive_ ranges, rather than the more
// typically C _exclusive_ range, for consistency with how these
// functions will be used.  In particular while we take care of the
// base1 to base0 mapping of most expressions, things that come in as
// index varibles (i, j, k) do not get subtracted as they are dealt
// with elsewhere.  So we can't just knock one off the "from" index
// and have things work because then a sum over an index (e.g.,
// sum(foo[i, ])) won't work because the limits would translate as
// {(i,i), (0, dim2)}.  Instead, using an inclusive range we can apply
// the transformation consistently and get {(i,i), (0, dim2-1)} which
// looks weird but should work everywhere.
double odin_sum1(double *x, int from_i, int to_i) {
  double tot = 0.0;
  for (int i = from_i; i <= to_i; ++i) {
    tot += x[i];
  }
  return tot;
}

double odin_sum2(double *x, int from_i, int to_i, int from_j, int to_j, int dim_1) {
  double tot = 0.0;
  for (int j = from_j; j <= to_j; ++j) {
    int jj = j * dim_1;
    for (int i = from_i; i <= to_i; ++i) {
      tot += x[i + jj];
    }
  }
  return tot;
}

double odin_sum3(double *x, int from_i, int to_i, int from_j, int to_j, int from_k, int to_k, int dim_1, int dim_2) {
  double tot = 0.0;
  const int dim_12 = dim_1 * dim_2;
  for (int k = from_k; k <= to_k; ++k) {
    int kk = k * dim_12;
    for (int j = from_j; j <= to_j; ++j) {
      int jj = j * dim_1 + kk;
      for (int i = from_i; i <= to_i; ++i) {
        tot += x[i + jj];
      }
    }
  }
  return tot;
}

void lagvalue_ds(double t, int *idx, int dim_idx, double *state) {
  typedef void (*lagvalue_type)(double, int*, int, double*);
  static lagvalue_type fun = NULL;
  if (fun == NULL) {
    fun = (lagvalue_type)R_GetCCallable("deSolve", "lagvalue");
  }
  return fun(t, idx, dim_idx, state);
}

void lagvalue_dde(double t, int *idx, size_t dim_idx, double *state) {
  typedef void (*lagvalue_type)(double, int*, size_t, double*);
  static lagvalue_type fun = NULL;
  if (fun == NULL) {
    fun = (lagvalue_type)R_GetCCallable("dde", "ylag_vec_int");
  }
  return fun(t, idx, dim_idx, state);
}

void odin_set_dim2(SEXP target, int nr, int nc) {
  SEXP dim = PROTECT(allocVector(INTSXP, 2));
  INTEGER(dim)[0] = nr;
  INTEGER(dim)[1] = nc;
  setAttrib(target, R_DimSymbol, dim);
  UNPROTECT(1);
}

void odin_set_dim3(SEXP target, int nr, int nc, int nz) {
  SEXP dim = PROTECT(allocVector(INTSXP, 3));
  INTEGER(dim)[0] = nr;
  INTEGER(dim)[1] = nc;
  INTEGER(dim)[2] = nz;
  setAttrib(target, R_DimSymbol, dim);
  UNPROTECT(1);
}

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
