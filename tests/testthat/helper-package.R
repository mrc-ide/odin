odin_create_package <- function(name, filenames, single_file=TRUE,
                                build=TRUE, verbose=TRUE) {
  pkg <- file.path(tempfile(), name)
  dir.create(pkg, FALSE, TRUE)
  for (f in c("DESCRIPTION", "NAMESPACE")) {
    writeLines(sprintf(readLines(file.path("pkg", f)), name),
               file.path(pkg, f))
  }
  dir.create(file.path(pkg, "inst", "odin"), FALSE, TRUE)
  file.copy(filenames, file.path(pkg, "inst", "odin"))
  odin_generate_package(pkg, single_file=single_file)
  if (build) {
    build_package(pkg, verbose)
  } else {
    pkg
  }
}

unload_package <- function(name) {
  if (name %in% loadedNamespaces()) {
    unloadNamespace(name)
  }
}

build_package <- function(path, verbose=TRUE) {
  name <- read.dcf(file.path(path, "DESCRIPTION"), "Package")[[1]]
  unload_package(name)
  if (name %in% .packages()) {
    detach(paste0("package:", name), unload=TRUE, character.only=TRUE)
  }

  lib <- tempfile()
  dir.create(lib)

  args <- c("CMD", "INSTALL", "-l", lib, path)
  Sys.setenv(R_TESTS="")
  system_out <- if (isTRUE(verbose)) "" else verbose
  ok <- system2(file.path(R.home(), "bin", "R"), args,
                stdout=system_out, stderr=system_out)
  if (ok != 0L) {
    stop("Error compiling package")
  }

  cleanup <- function() {
    unload_package(name)
    unlink(dirname(path), recursive=TRUE)
    unlink(lib, recursive=TRUE)
  }

  list(name=name,
       path=path,
       lib=lib,
       env=loadNamespace(name, lib.loc=lib),
       cleanup=cleanup)
}
