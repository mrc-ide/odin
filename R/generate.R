odin_ring_support <- function(package) {
  ## Annoyingly different from the version used in interpolate
  filter_includes <- function(filename) {
    x <- readLines(filename)
    x[!grepl("^#include\\s+", x, perl = TRUE)]
  }

  r_h <- system.file("include/ring/ring.h", package = "ring", mustWork = TRUE)
  r_c <- system.file("include/ring/ring.c", package = "ring", mustWork = TRUE)
  if (package) {
    decl <- filter_includes(r_h)
    defn <- filter_includes(r_c)
  } else {
    decl <- sprintf('#include "%s"', r_h)
    defn <- sprintf('#include "%s"', r_c)
  }
  list(declarations = decl, definitions = defn)
}

## Read a bunch of library functions.  The format here is important.
##
## This could be relaxed soon, though doing it correctly will require
## things like a proper C parser.  A more sensible route forward would
## be to allow, in addition, arbitrary functions to be listed with the
## inclusion of a header file.
read_user_c <- function(filename) {
  d <- readLines(filename)

  re1 <- "^[[:alnum:]_*]+ ([[:alnum:]_]+)(.+)"
  i1 <- grep(re1, d)
  i2 <- grep("^}$", d)
  if (length(i1) != length(i2)) {
    stop("Parse error for ", filename)
  }
  name <- sub(re1, "\\1", d[i1])
  defn <- setNames(vcapply(seq_along(i1), function(k)
    paste(d[i1[[k]]:i2[[k]]], collapse = "\n")), name)
  decl <- sub("^([^{]*?)\\s*\\{.*", "\\1;", defn)

  list(declarations = decl, definitions = defn, filename = filename)
}
