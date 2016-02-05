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
  if (is.loaded("derivs", base)) {
    dyn.unload(base)
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

  dyn <- normalizePath(paste0(base, .Platform$dynlib.ext))
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

  fmt <- paste("void derivs(int *neq, double *t, double *STATE,",
               "            double *D, double *yout, int *np) {",
               "%s",
               "}",
               sep="\n")

  order <- seq_along(derivs_rhs) - 1L
  start <- paste(sprintf("double %s = STATE[%dL];",
                         names(derivs_rhs), order), collapse="\n")
  rhs <- vcapply(derivs_rhs, deparse_str_c, USE.NAMES=FALSE)
  rhs <- paste(sprintf("D[%d] = %s;", order, rhs), collapse="\n")
  sprintf(fmt, indent(paste(start, rhs, sep="\n"), 2))
}

generate_initmod_c <- function(x) {
  ## Dummy for now:
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

## The next question is how much do we deal with list accessing
## initial conditions?
deparse_str_c <- function(x) {
  ## Do C translation here:
  ##   ^ -> pow
  ## or do I do that in the parse step?
  deparse_str(x)
}
