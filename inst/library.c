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

double odin_sum(double *x, int len) {
  double tot = 0.0;
  for (int i = 0; i < len; ++i) {
    tot += x[i];
  }
  return tot;
}

void lagvalue(double t, int *idx, int dim_idx, double *state) {
  /* typedef void (*lagvalue_type)(double, int*, int, double*); */
  /* static lagvalue_type fun = NULL; */
  /* if (fun == NULL) { */
  /*   fun = (lagvalue_type)R_GetCCallable("deSolve", "lagvalue"); */
  /* } */
  static void(*fun)(double, int*, int, double*) = NULL;
  if (fun == NULL) {
    fun = (void(*)(double, int*, int, double*))R_GetCCallable("deSolve", "lagvalue");
  }
  return fun(t, idx, dim_idx, state);
}
