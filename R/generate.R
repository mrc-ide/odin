## TODO: There are two ways of running this; one could generate the
## model on the fly, the other could write it out.
##
## Writing it out might be simpler.
odin_generate <- function(x, path=tempfile("odin_")) {
  ## TODO: may need to dump out a
  txt <- paste(c(generate_header_c(x),
                 generate_constants_c(x),
                 generate_initmod_c(x),
                 generate_derivs_c(x)), collapse="\n")
  ## TODO: this would be the model prefix here:
  dir.create(path, FALSE, TRUE)
  file <- file.path(path, "odin.c")
  writeLines(txt, file)
  file
}

odin_compile <- function(file) {
  file <- normalizePath(file, mustWork=TRUE)
  path <- dirname(file)
  owd <- setwd(dirname(file))
  on.exit(setwd(owd))

  base <- sub("\\.c$", "", basename(file))
  dyn <- paste0(base, .Platform$dynlib.ext)
  if (is.loaded("derivs", base)) {
    dyn.unload(dyn)
  }

  del <- dir(pattern=c("\\.o$", "\\.so$", "\\.dll$"))
  if (length(del) > 0L) {
    file.remove(del)
  }
  args <- c("CMD", "SHLIB", basename(file))
  ok <- system2(file.path(R.home(), "bin", "R"), args)
  if (ok != 0L) {
    stop("Error compiling ", file)
  }

  dyn.load(dyn)

  ## TODO: From this it would be nice to return information about the
  ## dynamically loaded functions so that using this from deSolve was
  ## easier.
  ##
  ## However, hold off on doing that until I see how this should fit
  ## into a package as there's more weirdness coming still.

  base
}

generate_constants_c <- function(x) {
  constants <- x$deriv$depends$constant
  sprintf("#define %s %s",
          names(constants),
          vcapply(constants, num_to_str, "digits17"))
}

generate_derivs_c <- function(x) {
  derivs_rhs <- lapply(x$deriv$rhs, "[[", "value")
  deps_t <- x$deriv$depends$time
  vars_rhs <- lapply(x$vars$rhs[deps_t], "[[", "value")

  ## Here, I do need to make sure that I can compile all the
  ## expressions into C.  That's a bit tricky because we don't have
  ## some operators in C.
  ##
  ## The only really nasty one is '%' which is not valid R (and %% is
  ## not valid C).  %/% is integer division in R, but in C that
  ## depends on the types.
  ##
  ## So for now assume we have that small subset of both languages
  ## that match up, and we'll fix it up later.
  ##
  ## TODO: Need to get some sort of sane naming scheme for the
  ## state/derivs argument naming pair, as well as for the derivs()
  ## function.

  fmt <- paste("void derivs(int *neq, double *t, double *ODIN_STATE,",
               "            double *ODIN_DERIVS, double *yout, int *np) {",
               "%s",
               "}",
               sep="\n")

  order <- seq_along(derivs_rhs) - 1L
  start <- paste(sprintf("const double %s = ODIN_STATE[%dL];",
                         names(derivs_rhs), order), collapse="\n")
  rhs <- vcapply(derivs_rhs, deparse_str_c, USE.NAMES=FALSE)
  rhs <- paste(sprintf("ODIN_DERIVS[%d] = %s;", order, rhs), collapse="\n")

  if (length(vars_rhs) > 0L) {
    rhs <- paste(c(sprintf("const double %s = %s;",
                           names(vars_rhs),
                           vcapply(vars_rhs, deparse_str_c, USE.NAMES=FALSE)),
                   rhs), collapse="\n")
  }

  sprintf(fmt, indent(paste(start, rhs, sep="\n"), 2))
}

generate_initmod_c <- function(x) {
  ## TODO: this should at least check that the number of parameters is
  ## correct!  That would look like:
  ##
  ##   int N = 3;
  ##   odeparms(&N, parms);
  ##
  ## If we want to fill parms.  Otherwise we have to use the full
  ## interface:
  ##
  ##    int N = 3;
  ##    DL_FUNC get_deSolve_gparms =
  ##      R_GetCCallable("deSolve", "get_deSolve_gparms");
  ##    SEXP rp = get_deSolve_gparms();
  ##    if (LENGTH(rp) != N) {
  ##      Rf_error("Incorrect number of parameters; expected %d");
  ##    }
  ##
  ## This is a little more complicated if we need to look up parameter
  ## lengths because we'd need to look the values of that up.
  ##
  ## NOTE: Dummy function for now.
  "void initmod(void(* odeparms) (int*, double *)) {\n}"
}

generate_header_c <- function(x) {
  ## TODO: conditionally include arbitrary C code and declare mapping
  ## for custom C code?
  ##
  ## TODO: BLAS?  Always link in?
  ##
  ## TODO: Custom names (to avoid collision when >1 is included).
  ## Prefix global parameters, derivs, initmod, etc.
  c("#include <R.h>",
    "#include <Rinternals.h>",
    ## "#include <Rext/dyn_load.h>"
    "void derivs(int *neq, double *t, double *STATE,",
    "            double *D, double *yout, int *np);",
    "void initmod(void(* odeparms) (int *, double *));")
}

generate_initial_conditions <- function(x) {
  ## This one is going to return an R function for now.

  if (length(x$initial$depends$user) > 0L) {
    stop("Not yet handled")
  }
  if (length(x$initial$depends$time) > 0L) {
    stop("Not yet handled")
  }

  ## TODO: This should go into parse or process, I think.
  msg <- setdiff(names(x$initial$lhs), names(x$deriv$rhs))
  if (length(msg) > 0L) {
    stop("Missing initial conditions for: %s", paste(msg, collapse=", "))
  }
  extra <- setdiff(names(x$deriv$rhs), names(x$initial$lhs))
  if (length(msg) > 0L) {
    stop("Extra initial conditions for: %s", paste(extra, collapse=", "))
  }
  ## TODO:
  if (!all(x$initial$types[names(x$deriv$rhs)] == TYPE_CONSTANT)) {
    stop("Non-constant initial conditions not yet handled")
  }
  resolve <- function() {
    vnapply(x$initial$rhs[names(x$deriv$rhs)], function(x) x$value)
  }
  function() {
    resolve()
  }
}
