## Almost every function in here generates exactly one function, except:
##
## * odin_generate2_struct(); generates one struct
## * odin_generate2_vars() and odin_generate2_unpack(); support
##   functions used in odin_generate2_deriv and odin_generate2_output
##   (factored out because output and deriv have similar structure).
##
## All of these functions take 'obj' as the first argument; this is
## the result of running odin_generate1().  None of these functions
## modify 'obj'.
##
## Every function that has any other argument has 'output=FALSE' as
## its only other argument; in this case they're unpacking/working
## with output rather than varaibles, as there is quite a bit of
## overlap there.
odin_generate2_struct <- function(obj) {
  types <- obj$types
  ret <- collector()
  ret$add("// Collect together all the parameters and transient memory")
  ret$add("// required to run the model in a struct.")
  ret$add("typedef struct %s {", obj$type_pars)
  ptr <- types$array | types$type == "interpolate_data"
  ret$add("  %s %s%s;", types$type, ifelse(ptr, "*", ""), types$name)
  ret$add("} %s;", obj$type_pars)
  ret$get()
}

odin_generate2_support_decls <- function(obj) {
  ret <- collector()
  ret$add("%s* %s_get_pointer(SEXP %s_ptr, int closed_error);",
          obj$type_pars, obj$info$base, obj$info$base)
  ret$add("SEXP %s_set_user(%s *%s, SEXP %s);",
          obj$info$base, obj$type_pars, obj$name_pars, USER)
  ret$get()
}

odin_generate2_support_defns <- function(obj) {
  ret <- collector()
  ret$add("%s* %s_get_pointer(SEXP %s_ptr, int closed_error) {",
          obj$type_pars, obj$info$base, obj$info$base)
  ret$add("  %s *%s = NULL;", obj$type_pars, obj$name_pars)
  ret$add("  if (TYPEOF(%s_ptr) != EXTPTRSXP) {", obj$info$base)
  ret$add('    Rf_error("Expected an external pointer");')
  ret$add("  }")
  ret$add("  %s = (%s*) R_ExternalPtrAddr(%s_ptr);",
          obj$name_pars, obj$type_pars, obj$info$base)
  ret$add("  if (!%s && closed_error) {", obj$name_pars)
  ret$add('    Rf_error("Pointer has been invalidated");')
  ret$add("  }")
  ret$add("  return %s;", obj$name_pars)
  ret$add("}")
  ret$get()
}

odin_generate2_library_fns <- function(obj) {
  dat <- read_user_c(system.file("library.c", package="odin"))
  fns <- obj$library_fns$get()
  if (any(grepl("^get_user_", fns))) {
    fns <- c(fns, "get_list_element")
  }
  fns <- unique(fns)
  ret <- list(declarations=c(unname(dat$declarations[fns]),
                             unname(obj$custom$declarations)),
              definitions=c(unname(dat$definitions[fns]),
                            unname(obj$custom$definitions)))
  ret
}

## TODO: For now, odin_interpolate_support is elsewhere.

## TODO: For now, odin_header(), odin_includes() elsewhere

odin_generate2_create <- function(obj) {
  ret <- collector()
  ret$add("// Create the pointer; this will establish the struct, allocate")
  ret$add("// memory for things that are constant size, and initialize")
  ret$add("// constant variables")
  ## NOTE: finalize definition in odin_generate2_finalize()
  ret$add("static void %s_finalize(SEXP %s_ptr);", obj$info$base, obj$info$base)
  ret$add("SEXP %s_create(SEXP %s, SEXP odin_use_dde) {", obj$info$base, USER)
  ret$add("  %s *%s = (%s*) Calloc(1, %s);",
          obj$type_pars, obj$name_pars, obj$type_pars, obj$type_pars)
  constant <- obj$constant$get()
  if (length(constant) > 0L) {
    ret$add(indent(constant, 2))
  }
  ret$add(
    "  SEXP %s_ptr = PROTECT(R_MakeExternalPtr(%s, R_NilValue, R_NilValue));",
    obj$info$base, obj$name_pars)
  ret$add("  R_RegisterCFinalizer(%s_ptr, %s_finalize);",
          obj$info$base, obj$info$base)
  ## NOTE: set user variables *after* creating the pointer and
  ## finaliser to avoid any memory leak in the case of set_user
  ## failing (as it throws on failure so the Free()'s would never
  ## happen).
  ret$add("  %s_set_user(%s, %s);", obj$info$base, obj$name_pars, USER)
  ret$add("  %s = INTEGER(odin_use_dde)[0];", obj$rewrite("odin_use_dde"))
  ret$add("  UNPROTECT(1);")
  ret$add("  return %s_ptr;", obj$info$base)
  ret$add("}")
  ret$get()
}

odin_generate2_user <- function(obj) {
  ret <- collector()
  ret$add("// Set user-supplied parameter values.")
  ret$add("SEXP %s_set_user(%s *%s, SEXP %s) {",
          obj$info$base, obj$type_pars, obj$name_pars, USER)
  user <- obj$user$get()
  if (length(user) > 0L) {
    ret$add(indent(user, 2))
  }
  ret$add("  return R_NilValue;")
  ret$add("};")
  ret$add("// Wrapper around this for use from R.")
  ret$add("SEXP r_%s_set_user(SEXP %s_ptr, SEXP %s) {",
          obj$info$base, obj$info$base, USER)
  ret$add("  %s *%s = %s_get_pointer(%s_ptr, 1);",
          obj$type_pars, obj$name_pars, obj$info$base, obj$info$base)
  ret$add("  %s_set_user(%s, %s);", obj$info$base, obj$name_pars, USER)
  ret$add("  return R_NilValue;")
  ret$add("};")
  ret$get()
}

odin_generate2_finalize <- function(obj) {
  ret <- collector()
  ret$add("// Arrange to free all memory we have allocated")
  ret$add("// This is called by R automatically when the pointer is")
  ret$add("// garbage collected (i.e., when all objects holding the pointer")
  ret$add("// go out of scope")
  ## NOTE: declaration for this is made in odin_generate2_create()
  ret$add("void %s_finalize(SEXP %s_ptr) {", obj$info$base, obj$info$base)
  ret$add("  %s *%s = %s_get_pointer(%s_ptr, 0);",
          obj$type_pars, obj$name_pars, obj$info$base, obj$info$base)
  ret$add("  if (%s_ptr) {", obj$info$base)
  free <- obj$free$get()
  if (length(free) > 0L) {
    ret$add(indent(free, 4))
  }
  ret$add("    Free(%s);", obj$name_pars)
  ret$add("    R_ClearExternalPtr(%s_ptr);", obj$info$base)
  ret$add("  }")
  ret$add("}")
  ret$get()
}

odin_generate2_initial <- function(obj) {
  ret <- collector()
  ret$add("SEXP %s_initialise(SEXP %s_ptr, SEXP %s_ptr) {",
          obj$info$base, obj$info$base, TIME)
  ret$add("  %s *%s = %s_get_pointer(%s_ptr, 1);",
          obj$type_pars, obj$name_pars, obj$info$base, obj$info$base)

  initial <- obj$initial$get()

  if (obj$info$has_delay || length(initial) > 0L) {
    if (obj$info$has_delay || obj$info$initial_stage >= STAGE_TIME) {
      ret$add("  const double %s = REAL(%s_ptr)[0];", TIME, TIME)
    }
    if (obj$info$has_delay) {
      ret$add("  %s = %s;", obj$rewrite(initial_name(TIME)), TIME)
    }

    ## Dependencies of any initial expressions, filtered by time dependency:
    time <- obj$time$get()
    time <- time[names(time) %in% obj$initial_info$used$exprs]
    if (length(time) > 0L) {
      ret$add(indent(time, 2))
    }

    ## And time-sensitive initial expressions
    if (length(initial) > 0L) {
      ret$add(indent(initial, 2))
    }
  }

  vars_info <- obj$variable_info
  ## It's possible this bit should be factored out into a separate function?
  ret$add("  SEXP %s = PROTECT(allocVector(REALSXP, %s));",
          STATE, obj$rewrite(obj$variable_info$total_use))
  copy <- character(vars_info$n)
  i <- vars_info$is_array
  nm_initial <- vcapply(initial_name(vars_info$order), obj$rewrite)
  offset <- vcapply(vars_info$offset_use, obj$rewrite)
  copy[i] <- sprintf("  memcpy(REAL(%s) + %s, %s, %s * sizeof(double));",
                     STATE, offset[i], nm_initial[i],
                     vcapply(vars_info$len[i], obj$rewrite))
  copy[!i] <- sprintf("  REAL(%s)[%s] = %s;",
                      STATE, offset[!i], nm_initial[!i])
  ret$add(copy)
  if (obj$info$has_output) {
    ret$add('  setAttrib(%s, install("%s_len"), ScalarInteger(%s));',
            STATE, OUTPUT, obj$rewrite(obj$output_info$total_use))
  }
  ret$add("  UNPROTECT(1);")
  ret$add("  return %s;", STATE)
  ret$add("}")
  ret$get()
}

## OK, this one is slightly complicated because there are *three*
## forms of the derivative function.  The base one (that this does)
## returns void and takes a pointer.
odin_generate2_deriv <- function(obj) {
  info <- obj$variable_info

  ret <- collector()
  ret$add(
    "void %s_deriv(%s *%s, double %s, double *%s, double *%s, double *%s) {",
    obj$info$base, obj$type_pars, obj$name_pars, TIME, STATE, DSTATEDT, OUTPUT)

  ret$add(indent(odin_generate2_vars(obj), 2))
  ret$add(indent(odin_generate2_unpack(obj), 2))

  time <- obj$time$get()
  if (length(time) > 0L) {
    ret$add(indent(time, 2))
  }

  ## The conditional here means that we'll be able to be compatible
  ## with both dde and deSolve, as they totally differ in how output
  ## variables are computed.  We pass a NULL through for output for
  ## dde so this will skip pretty happily.
  output <- obj$output$get()
  if (length(output) > 0L) {
    ret$add("  if (%s != NULL) {", OUTPUT)
    ret$add(indent(odin_generate2_unpack(obj, TRUE), 4))
    ret$add(indent(output, 4))
    ret$add("  }")
  }
  ret$add("}")
  ret$get()
}

odin_generate2_deriv_r <- function(obj) {
  ret <- collector()
  ret$add("SEXP r_%s_deriv(SEXP %s_ptr, SEXP %s, SEXP %s) {",
          obj$info$base, obj$info$base, TIME, STATE)
  ret$add("  SEXP %s = PROTECT(allocVector(REALSXP, LENGTH(%s)));",
          DSTATEDT, STATE)
  ret$add("  %s *%s = %s_get_pointer(%s_ptr, 1);",
          obj$type_pars, obj$name_pars, obj$info$base, obj$info$base)

  if (obj$info$has_output) {
    ret$add("  SEXP %s_ptr = PROTECT(allocVector(REALSXP, %s));",
            OUTPUT, obj$rewrite(obj$output_info$total_use))
    ret$add('  setAttrib(%s, install("%s"), %s_ptr);', DSTATEDT, OUTPUT, OUTPUT)
    ret$add("  double *%s = REAL(%s_ptr);", OUTPUT, OUTPUT)
    np <- 2L
  } else {
    ret$add("  double *%s = NULL;", OUTPUT)
    np <- 1L
  }

  ret$add("  %s_deriv(%s, REAL(%s)[0], REAL(%s), REAL(%s), %s);",
          obj$info$base, obj$name_pars, TIME, STATE, DSTATEDT, OUTPUT)
  ret$add("  UNPROTECT(%d);", np)
  ret$add("  return %s;", DSTATEDT)
  ret$add("}")
  ret$get()
}

odin_generate2_contents <- function(obj) {
  types <- obj$types
  len <- nrow(types)

  rtype <- c(int="INTSXP", double="REALSXP")
  raccess <- c(int="INTEGER", double="REAL")

  ret <- collector()
  ret$add("// Translate all elements in the struct back to R")
  ret$add("// This will mostly be useful for debugging.")
  ret$add("SEXP %s_contents(SEXP %s_ptr) {", obj$info$base, obj$info$base)
  ret$add("  %s *%s = %s_get_pointer(%s_ptr, 1);",
          obj$type_pars, obj$name_pars, obj$info$base, obj$info$base)
  ret$add("  SEXP %s = PROTECT(allocVector(VECSXP, %d));",
          STATE, len)

  for (i in seq_len(len)) {
    name <- types$name[[i]]
    type <- types$type[[i]]
    array <- types$array[[i]]
    if (type == "interpolate_data") {
      ## Can't do anything with these.
      next
    }
    if (array > 0L) {
      name_dim <- array_dim_name(name)
      ret$add("  SET_VECTOR_ELT(%s, %d, allocVector(%s, %s));",
                   STATE, i - 1L, rtype[[type]], obj$rewrite(name_dim))
      ret$add("  memcpy(%s(VECTOR_ELT(%s, %d)), %s, %s * sizeof(%s));",
              raccess[[type]], STATE, i - 1L, obj$rewrite(name),
              obj$rewrite(name_dim), type)
      if (array > 1L) {
        ret$add("  odin_set_dim%d(VECTOR_ELT(%s, %d), %s);",
                array, STATE, i - 1L,
                paste(vcapply(seq_len(array), function(j)
                  obj$rewrite(array_dim_name(name, j))), collapse=", "))
      }
    } else {
      type <- if (type == "int") "Integer" else "Real"
      ret$add("  SET_VECTOR_ELT(%s, %d, Scalar%s(%s));",
              STATE, i - 1L, type, obj$rewrite(name))
    }
  }

  ret$add("  SEXP %s_names = PROTECT(allocVector(STRSXP, %d));",
          STATE, len)
  ret$add('  SET_STRING_ELT(%s_names, %d, mkChar("%s"));',
          STATE, seq_len(len) - 1L, types$name)

  ret$add("  setAttrib(%s, R_NamesSymbol, %s_names);", STATE, STATE)
  ret$add("  UNPROTECT(2);")
  ret$add("  return %s;", STATE)
  ret$add("}")

  ret$get()
}

odin_generate2_order <- function(obj, output=FALSE) {
  ret <- collector()
  if (output) {
  ret$add("// Report back to R information on output variable ordering")
  ret$add("// Like the variable order above, but for any output vars")
  ret$add("// If no output variables are used, return an R NULL")
  } else {
    ret$add("// Report back to R information on variable ordering")
    ret$add("// The reported information includes position and length of each")
    ret$add("// variable, from which offset, etc, can be worked out.")
  }
  ret$add("SEXP %s_%s_order(SEXP %s_ptr) {",
          obj$info$base, if (output) "output" else "variable", obj$info$base)

  ## Early exit if we have nothing to generate:
  if (output && !obj$info$has_output) {
    ret$add("  return R_NilValue;", STATE)
    ret$add("}")
    return(ret$get())
  }

  info <- obj[[if (output) "output_info" else "variable_info"]]
  if (any(info$is_array)) {
    ret$add("  %s *%s = %s_get_pointer(%s_ptr, 1);",
            obj$type_pars, obj$name_pars, obj$info$base, obj$info$base)
    if (max(info$array) > 1L) {
      ret$add("  int *tmp;")
    }
  }
  ret$add("  SEXP %s_len = PROTECT(allocVector(VECSXP, %d));",
          STATE, info$n)
  ret$add("  SEXP %s_names = PROTECT(allocVector(STRSXP, %d));",
          STATE, info$n)

  for (i in seq_len(info$n)) {
    nd <- info$array[[i]]
    if (nd == 0L) {
      ret$add("  SET_VECTOR_ELT(%s_len, %s, R_NilValue);", STATE, i - 1L)
    } else if (nd == 1L) {
      ret$add("  SET_VECTOR_ELT(%s_len, %s, ScalarInteger(%s));",
              STATE, i - 1L, obj$rewrite(info$len[[i]]))
    } else {
      ret$add("  SET_VECTOR_ELT(%s_len, %s, allocVector(INTSXP, %d));",
              STATE, i - 1L, nd)
      ret$add("  tmp = INTEGER(VECTOR_ELT(%s_len, %s));", STATE, i - 1L)
      for (j in seq_len(nd)) {
        ret$add("  tmp[%d] = %s;", j - 1L,
                obj$rewrite(array_dim_name(info$order[[i]], j)))
      }
    }
    ret$add("  SET_STRING_ELT(%s_names, %d, mkChar(\"%s\"));",
            STATE, i - 1L, info$order[[i]])
  }
  ret$add("  setAttrib(%s_len, R_NamesSymbol, %s_names);", STATE, STATE)
  ret$add("  UNPROTECT(2);")
  ret$add("  return %s_len;", STATE)
  ret$add("}")
  ret$get()
}

odin_generate2_output <- function(obj) {
  if (!obj$info$has_output) {
    return(NULL)
  }
  info <- obj$output_info

  ret <- collector()
  ret$add(
    "void %s_output(%s *%s, double %s, double *%s, double *%s) {",
    obj$info$base, obj$type_pars, obj$name_pars, TIME, STATE, OUTPUT)

  ## 1. variables that we need to use
  ret$add(indent(odin_generate2_vars(obj, TRUE), 2))
  ret$add(indent(odin_generate2_unpack(obj, TRUE), 2))

  ## 2. dependent calculations
  time <- obj$time$get()
  time <- time[names(time) %in% obj$output_info$used$exprs]
  if (length(time) > 0L) {
    ret$add(indent(time, 2))
  }

  ## 3. the actual output calculations:
  ret$add(indent(obj$output$get(), 2))
  ret$add("}")
  ret$get()
}

odin_generate2_interpolate_t <- function(obj) {
  ret <- collector()
  ret$add("// Report back to R information about interpolating functions")
  ret$add("SEXP %s_interpolate_t(SEXP %s_ptr) {", obj$info$base, obj$info$base)
  if (obj$info$has_interpolate) {
    ret$add("  %s *%s = %s_get_pointer(%s_ptr, 1);",
            obj$type_pars, obj$name_pars, obj$info$base, obj$info$base)
    dat <- unique(obj$interpolate$get())
    dat_type <- vcapply(dat, "[[", "interpolation_type")
    dat_time <- vcapply(dat, "[[", "t")
    tmp <- sort(tapply(dat_type != "constant", dat_time, any), decreasing=TRUE)

    ret$add("  SEXP ret = PROTECT(allocVector(REALSXP, 2));")
    ret$add("  double *r = REAL(ret);")
    v <- names(tmp)[[1L]]
    ret$add("  r[0] = %s[0];", obj$rewrite(v))
    if (tmp[[v]] > 0L) {
      ret$add("  r[1] = %s[%s - 1];",
              obj$rewrite(v), obj$rewrite(array_dim_name(v)))
    } else {
      ret$add("  r[1] = NA_REAL;")
    }
    for (v in names(tmp)[-1]) {
      ret$add("  r[0] = min(r[0], %s[0]);", obj$rewrite(v))
      if (tmp[[v]] > 0) {
        ret$add("  r[1] = max(r[1], %s[%s - 1]);",
                obj$rewrite(v), obj$rewrite(array_dim_name(v)))
      }
    }
    ret$add("  UNPROTECT(1);")
    ret$add("  return ret;")
  } else {
    ret$add("  return R_NilValue;")
  }
  ret$add("}")
  ret$get()
}

odin_generate2_desolve <- function(obj) {
  ret <- collector()
  ret$add("// deSolve interface")
  ret$add("// Global variable set on initmod, as per deSolve design")
  ret$add("static %s *%s;", obj$type_pars, obj$name_pars)
  ret$add("void %s_ds_initmod(void(* odeparms) (int *, double *)) {",
          obj$info$base)
  ret$add('  DL_FUNC get_deSolve_gparms = R_GetCCallable("deSolve", "get_deSolve_gparms");')
  ret$add("  %s = %s_get_pointer(get_deSolve_gparms(), 1);",
          obj$name_pars, obj$info$base)
  ret$add("}")
  ret$add("void %s_ds_derivs(int *neq, double *%s, double *%s,",
          obj$info$base, TIME, STATE)
  ret$add("%sdouble *%s, double *%s, int *np) {",
          strrep(nchar(obj$info$base) + 15L), DSTATEDT, OUTPUT)
  ret$add("  %s_deriv(%s, *%s, %s, %s, %s);",
          obj$info$base, obj$name_pars, TIME, STATE, DSTATEDT, OUTPUT)
  ret$add("}")
  ret$get()
}

odin_generate2_dde <- function(obj) {
  ret <- collector()
  ret$add("// dde interface")
  ret$add("void %s_dde_derivs(size_t n_eq, double %s, double *%s,",
          obj$info$base, TIME, STATE)
  ret$add("%sdouble *%s, void *%s) {",
          strrep(nchar(obj$info$base) + 17L), DSTATEDT, obj$name_pars)
  ret$add("  %s_deriv((%s*)%s, %s, %s, %s, NULL);",
          obj$info$base, obj$type_pars, obj$name_pars, TIME, STATE, DSTATEDT)
  ret$add("}")

  if (obj$info$has_output) {
    ## This needs working through because we don't want to get the
    ## output at every derivative calculation (that's wasteful), and
    ## because the dde solver does not actually stop at all the
    ## requested points it does need to be done in two stages.  So
    ## this will require some extra faff with generating output
    ## functions, which requires another trip through the DAG too.

    ## Here, we'll need to have done some output processing.
    ret$add("\nvoid %s_dde_output(size_t n_eq, double %s, double *%s,",
            obj$info$base, TIME, STATE)
    ret$add("%ssize_t dim_%s, double *%s, void *%s) {",
            strrep(nchar(obj$info$base) + 17L), OUTPUT, OUTPUT, obj$name_pars)
    ret$add("  %s_output((%s*)%s, %s, %s, %s);",
            obj$info$base, obj$type_pars, obj$name_pars, TIME, STATE, OUTPUT)
    ret$add("}")
  }
  ret$get()
}

odin_generate2_info <- function(obj) {
  info <- obj$info
  ret <- collector()
  ret$add("// Report back to R some key features of the system of ODEs")
  ret$add("// These are truely constant features of the model -- not to do")
  ret$add("// with any specific realisation of the model, so we don't need")
  ret$add("// or want a pointer here.  Things like output length, variable")
  ret$add("// length etc might vary depending on parameters used to generate")
  ret$add("// the model so we'll pull those elsewhere")
  ret$add("SEXP %s_info() {", obj$info$base)
  ret$add("  SEXP ret = PROTECT(allocVector(VECSXP, %d));", length(info))
  ret$add("  SEXP nms = PROTECT(allocVector(STRSXP, %d));", length(info))
  as_vector <- vlapply(info, function(x)
                       length(x) > 1 || (length(x) > 0 && !is.null(names(x))))
  if (any(as_vector)) {
    ret$add("  SEXP tmp;")
  }
  for (i in seq_along(info)) {
    x <- info[[i]]
    ret$add('  SET_STRING_ELT(nms, %d, mkChar("%s"));',
            i - 1L, names(info)[[i]])
    if (length(x) == 0L) {
      ret$add('  SET_VECTOR_ELT(ret, %d, R_NilValue);', i - 1L)
    } else if (!as_vector[[i]]) {
      ## The names length check is required to avoid a weirdness in
      ## the R API here:
      ## https://stat.ethz.ch/pipermail/r-devel/2014-October/070010.html
      if (is.character(x)) {
        ret$add('  SET_VECTOR_ELT(ret, %d, mkString("%s"));', i - 1L, x)
      } else {
        fn <- if (is.logical(x)) "ScalarLogical" else "ScalarInteger"
        ret$add('  SET_VECTOR_ELT(ret, %d, %s(%d));',
                i - 1L, fn, as.integer(x))
      }
    } else {
      sxp <- switch(storage.mode(x),
                    logical="LGLSXP",
                    character="STRSXP")
      ret$add('  tmp = PROTECT(allocVector(%s, %d));', sxp, length(x))
      j <- seq_along(x) - 1L
      if (is.character(x)) {
        ret$add('  SET_STRING_ELT(tmp, %d, mkChar("%s"));', j, x)
      } else {
        ret$add('  INTEGER(tmp)[%d] = %d;', j, as.integer(x))
      }
      ret$add('  SET_VECTOR_ELT(ret, %d, tmp);', i - 1L)
      ret$add('  UNPROTECT(1);')
    }
    if (length(x) > 0L && !is.null(names(x))) {
      if (length(x) == 1L) {
        ret$add(
          '  setAttrib(VECTOR_ELT(ret, %d), R_NamesSymbol, mkString("%s"));',
          i - 1L, names(x))
      } else {
        ret$add('  tmp = PROTECT(allocVector(STRSXP, %d));', length(x))
        ret$add('  SET_STRING_ELT(tmp, %d, mkChar("%s"));', j, names(x))
        ret$add('  setAttrib(VECTOR_ELT(ret, %d), R_NamesSymbol, tmp);',
                i - 1L)
        ret$add('  UNPROTECT(1);')
      }
    }
  }
  ret$add("  setAttrib(ret, R_NamesSymbol, nms);")
  ret$add("  UNPROTECT(2);")
  ret$add("  return ret;")
  ret$add("}")
  ret$get()
}

## Helper functions here:
odin_generate2_vars <- function(obj, output=FALSE) {
  info <- obj$variable_info

  if (output) {
    used <- info$order %in% obj$output_info$used$output
  } else {
    used <- info$used
  }

  ret <- collector()
  i <- used & !info$is_array
  if (any(i)) {
    ret$add("double %s = %s[%s];", info$order[i], STATE,
            vcapply(info$offset_use[i], obj$rewrite))
  }
  i <- used & info$is_array
  if (any(i)) {
    ret$add("double *%s = %s%s;", info$order[i], STATE,
            vcapply(info$offset_use[i], odin_generate2_offset, obj$rewrite))
  }
  ret$get()
}

odin_generate2_unpack <- function(obj, output=FALSE) {
  info <- obj[[if (output) "output_info" else "variable_info"]]
  if (any(info$is_array)) {
    sprintf("double *%s_%s = %s%s;",
            if (output) "output" else "deriv",
            info$order[info$is_array],
            if (output) OUTPUT else DSTATEDT,
            vcapply(info$offset_use[info$is_array], odin_generate2_offset,
                    obj$rewrite))
  } else {
    character(0)
  }
}

odin_generate2_offset <- function(x, rewrite) {
  if (identical(x, 0L)) {
    ""
  } else {
    sprintf(" + %s", rewrite(x))
  }
}
