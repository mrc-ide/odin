## This will be the version for building on the fly; we'll need a
## different version for a package; the package version will not do
## the compilation, and will write out perhaps a different generator?
odin_build <- function(path, dest=".", verbose=TRUE, load=TRUE) {
  dat <- odin_parse(path)
  path <- odin_generate(dat, dest)
  dll <- compile(path, verbose, load)
  if (load) {
    ode_system_generator(dll)
  } else {
    path
  }
}

compile <- function(filename, verbose=TRUE, load=TRUE) {
  if (isTRUE(verbose)) {
    verbose <- ""
  }
  Sys.setenv(R_TESTS="")
  owd <- setwd(dirname(filename))
  on.exit(setwd(owd))
  base <- sub("\\.c$", "", basename(filename))
  ok <- system2(file.path(R.home(), "bin", "R"),
                c("CMD", "SHLIB", basename(filename)),
                stdout=verbose, stderr=verbose)
  if (ok != 0L) {
    stop("Error compiling source")
  }
  if (load) {
    dyn.load(paste0(base, .Platform$dynlib.ext))
  }
  base
}
