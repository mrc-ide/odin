% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odin_package.R
\name{odin_package}
\alias{odin_package}
\title{Create odin model in a package}
\usage{
odin_package(path_package)
}
\arguments{
\item{path_package}{Path to the package root (the directory that
contains \code{DESCRIPTION})}
}
\description{
Create an odin model within an existing package.
}
\details{
I am resisting the urge to actually create the package here.
There are better options than I can come up with; for example
\code{devtools::create}, \code{pkgkitten::kitten}, \code{mason::mason}, or
creating \code{DESCRIPTION} files using \code{desc}.  What is required here
is that your package:
\itemize{
\item Lists \code{odin} in \verb{Imports:}
\item Includes \verb{useDynLib(<your package name>)} in
\code{NAMESPACE} (possibly via a roxygen comment \verb{@useDynLib <your package name>}
\item To avoid a NOTE in \verb{R CMD check}, import something from
\code{odin} in your namespace (e.g., \code{importFrom("odin", "odin")}s
or roxygen \verb{@importFrom(odin, odin)}
}

Point this function at the package root (the directory containing
\code{DESCRIPTION} and it will write out files \code{src/odin.c}
and \code{odin.R}.  These files will be overwritten without
warning by running this again.
}
\examples{
path <- tempfile()
dir.create(path)

src <- system.file("examples/package", package = "odin", mustWork = TRUE)
file.copy(src, path, recursive = TRUE)
pkg <- file.path(path, "package")

# The package is minimal:
dir(pkg)

# But contains odin files in inst/odin
dir(file.path(pkg, "inst/odin"))

# Compile the odin code in the package
odin::odin_package(pkg)

# Which creates the rest of the package structure
dir(pkg)
dir(file.path(pkg, "R"))
dir(file.path(pkg, "src"))
}
