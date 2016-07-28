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
##' @export
odin_generate_package <- function(path_package, filenames=NULL,
                                  single_file=TRUE) {
  if (!file.exists(file.path(path_package, "DESCRIPTION"))) {
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
    stop("Input files not found:", paste(filenames[msg], collapse=", "))
  }

  dat <- lapply(filenames, function(f)
    odin_generate(odin_parse(f, "file"), package=TRUE))

  library_fns <- combine_library(dat)
  code <- vcapply(dat, "[[", "code")
  base <- vcapply(dat, "[[", "base")
  info <- lapply(dat, "[[", "info")
  struct <- lapply(dat, "[[", "struct")

  ## This is pretty tricky because the order here matters a lot
  ## (mostly because I never did a good job of forward declaring the
  ## struct.  The model struct needs to be defined after the
  ## interpolation struct.
  has_interpolate <- any(vlapply(info, "[[", "has_interpolate"))
  header <- c(odin_header(), odin_includes())

  if (has_interpolate) {
    interpolate <- odin_interpolate_support()
    header <- c(header, "\n", interpolate$types)
    library_fns$declarations <-
      c(library_fns$declarations, "\n", interpolate$declarations)
    library_fns$definitions <-
      c(library_fns$definitions, "\n", interpolate$definitions)
  }

  dir.create(file.path(path_package, "R"), FALSE)
  dir.create(file.path(path_package, "src"), FALSE)

  writel <- function(x, file) {
    writeLines(x, file.path(path_package, "src", file))
  }

  if (single_file) {
    struct[-1] <- lapply(struct[-1], function(x) x[!grepl("^//", x)])
    writel(c(header, "\n",
             unlist(struct), "\n",
             library_fns$declarations, "\n",
             code, "\n",
             library_fns$definitions), "odin.c")
  } else {
    writel(c(header, "\n", library_fns$definitions), "odin.c")
    for (i in seq_along(filenames)) {
      writel(c(header, "\n",
               struct[[i]], "\n",
               library_fns$declarations, "\n",
               code[[i]]), sprintf("odin_%s.c", base[[i]]))
    }
  }

  r_code <- sprintf(
    'delayedAssign("%s", odin::ode_system_generator(.packageName, "%s"))',
    base, base)
  writeLines(c(sub("^//", "##", odin_header()), r_code),
             file.path(path_package, "R", "odin.R"))
}

combine_library <- function(dat) {
  decl <- unlist(lapply(dat, function(x) x$library_fns$declarations))
  keep <- !duplicated(decl)
  decl <- decl[keep]
  defn <- unlist(lapply(dat, function(x) x$library_fns$definitions))[keep]
  list(declarations=decl, definitions=defn)
}
