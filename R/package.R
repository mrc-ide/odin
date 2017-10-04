##' Create an odin model within an existing package.
##'
##' I am resisiting the urge to actually create the package here.
##' There are better options than I can come up with; for example
##' \code{devtools::create}, \code{pkgkitten::kitten},
##' \code{mason::mason}, or creating \code{DESCRIPTION} files using
##' \code{desc}.  What is required here is that your package:
##'
##' \itemize{
##' \item{Lists \code{odin} in \code{Imports:}}
##' \item{Includes \code{useDynLib{<your package name>}} in
##'   \code{NAMESPACE} (possibly via a roxygen comment \code{@useDynLib
##'   <your package name>}}
##' \item{To avoid a NOTE in \code{R CMD check}, import something from
##'   \code{odin} in your namespace (e.g., \code{importFrom("odin", "odin")}
##'   or roxygen \code{@importFrom(odin, odin)}}
##' }
##'
##' Point this function at the package root (the directory containing
##' \code{DESCRIPTION} and it will, by default, write out files
##' \code{src/odin.c} and \code{odin.R}.  These files will be
##' overwritten without warning by running this again.
##'
##' The generated models are activated in the package using
##' \code{\link{delayedAssign}}.  This might prove challenging to
##' document (especially using roxygen) but hopefully something will
##' work out.
##'
##' Apart from the documentation, the generated code \emph{should}
##' pass R CMD check without issue.
##'
##' @title Create odin model in a package
##' @param path_package Path to the package root (the directory that
##'   contains \code{DESCRIPTION})
##' @param filenames A character vector of filenames containing odin
##'   sources.  Alternatively, all .R files within the directory
##'   \code{inst/odin} will be used.
##'
##' @param single_file Use a single file for all odin C code.  If
##'   \code{FALSE}, then one .c file will be generated for each model
##'   listed in \code{filenames}.
##'
##' @export
odin_package <- function(path_package, filenames=NULL, single_file=TRUE) {
  desc <- file.path(path_package, "DESCRIPTION")
  if (!file.exists(desc)) {
    stop("Did not find package at ", path_package)
  }
  if (is.null(filenames)) {
    inst_odin <- file.path(path_package, "inst/odin")
    if (!is_directory(inst_odin)) {
      stop("If 'filenames' is not given, inst/odin must exist")
    }
    filenames <- dir(inst_odin, pattern="\\.[Rr]", full.names=TRUE)
  }
  if (length(filenames) == 0L) {
    stop("At least one filename must be given")
  }
  msg <- !file.exists(filenames)
  if (any(msg)) {
    what <- ngettext(length(msg), "file", "files")
    stop(sprintf("Input %s not found: %s",
                 what, paste(filenames[msg], collapse=", ")))
  }
  dup <- duplicated(filenames)
  if (any(dup)) {
    dups <- unique(filenames[dup])
    what <- ngettext(length(msg), "file", "files")
    stop(sprintf("Duplicate %s: %s", what, paste(dups, collapse=", ")))
  }

  dat <- lapply(filenames, function(f)
    odin_generate(odin_parse(f), package=TRUE))

  library_fns <- combine_library(dat)
  struct <- lapply(dat, "[[", "struct")
  code <- vcapply(dat, "[[", "code")
  info <- lapply(dat, "[[", "info")
  base <- vcapply(info, "[[", "base")

  ## We can only generate the R code once we have the dll name
  name <- as.vector(read.dcf(desc, "Package"))
  if (is.na(name)) {
    ## This might be overly cautious.
    stop("Failed to get package name from DESCRIPTION")
  }
  r_code <- lapply(info, odin_generate_r, name)

  header <- c(odin_header(), odin_includes())

  dir.create(file.path(path_package, "R"), FALSE)
  dir.create(file.path(path_package, "src"), FALSE)

  writel <- function(x, file) {
    x <- x[lengths(x) > 0]
    txt <- paste(vcapply(x, paste, collapse="\n"), collapse="\n\n")
    writeLines(txt, file.path(path_package, "src", file))
  }

  if (single_file) {
    struct[-1] <- lapply(struct[-1], function(x) x[!grepl("^//", x)])
    writel(list(header,
                unlist(struct),
                library_fns$declarations,
                code,
                library_fns$definitions),
           "odin.c")
  } else {
    writel(list(header,
                library_fns$declarations,
                library_fns$definitions),
                "odin.c")
    for (i in seq_along(filenames)) {
      writel(list(header,
                  library_fns$declarations,
                  struct[[i]],
                  code[[i]]), sprintf("odin_%s.c", base[[i]]))
    }
  }

  header_r <- sub("^//", "##", odin_header())
  if (single_file) {
    filename_r <- file.path(path_package, "R", "odin.R")
    writeLines(c(header_r, unlist(r_code)), filename_r)
  } else {
    filenames_r <- file.path(path_package, "R", sprintf("odin_%s.R", base))
    for (i in seq_along(filenames)) {
      writeLines(c(header_r, r_code[[i]]), filenames_r[[i]])
    }
  }
}

combine_library <- function(dat) {
  decl <- unlist(lapply(dat, function(x) x$library_fns$declarations))
  keep <- !duplicated(decl)
  decl <- decl[keep]
  defn <- unlist(lapply(dat, function(x) x$library_fns$definitions))[keep]
  list(declarations=decl, definitions=defn)
}
