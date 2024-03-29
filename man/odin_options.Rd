% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odin_options.R
\name{odin_options}
\alias{odin_options}
\title{Odin options}
\usage{
odin_options(verbose = NULL, target = NULL, workdir = NULL,
  validate = NULL, pretty = NULL, skip_cache = NULL,
  compiler_warnings = NULL, no_check_unused_equations = NULL,
  rewrite_dims = NULL, rewrite_constants = NULL, debug_enable = NULL,
  substitutions = NULL, options = NULL)
}
\arguments{
\item{verbose}{Logical scalar indicating if the compilation should
be verbose.  Defaults to the value of the option
\code{odin.verbose} or \code{FALSE} otherwise.}

\item{target}{Compilation target.  Options are "c", "r" or "js",
defaulting to the option \code{odin.target} or "c" otherwise.}

\item{workdir}{Directory to use for any generated files.  This is
only relevant for the "c" target.  Defaults to the value of the
option \code{odin.workdir} or \code{\link[=tempdir]{tempdir()}} otherwise.}

\item{validate}{Validate the model's intermediate representation
against the included schema.  Normally this is not needed and is
intended primarily for development use.  Defaults to the value
of the option \code{odin.validate} or \code{FALSE} otherwise.}

\item{pretty}{Pretty-print the model's intermediate
representation.  Normally this is not needed and is intended
primarily for development use.  Defaults to the value of the
option \code{odin.pretty} or \code{FALSE} otherwise.}

\item{skip_cache}{Skip odin's cache.  This might be useful if the
model appears not to compile when you would expect it to.
Hopefully this will not be needed often.  Defaults to the option
\code{odin.skip_cache} or \code{FALSE} otherwise.}

\item{compiler_warnings}{Previously this attempted detection of
compiler warnings (with some degree of success), but is
currently ignored. This may become supported again in a future
version depending on underlying support in pkgbuild.}

\item{no_check_unused_equations}{If \code{TRUE}, then don't print
messages about unused variables.  Defaults to the option
\code{odin.no_check_unused_equations} or \code{FALSE} otherwise.}

\item{rewrite_dims}{Logical, indicating if odin should try and
rewrite your model dimensions (if using arrays). If \code{TRUE} then
we replace dimensions known at compile-time with literal
integers, and those known at initialisation with simplified and
shared expressions. You may get less-comprehensible error
messages with this option set to \code{TRUE} because parts of the
model have been effectively evaluated during processing.}

\item{rewrite_constants}{Logical, indicating if odin should try
and rewrite \emph{all} constant scalars. This is a superset of
\code{rewrite_dims} and may be slow for large models. Doing this will
make your model less debuggable; error messages will reference
expressions that have been extensively rewritten, some variables
will have been removed entirely or merged with other identical
expressions, and the generated code may not be obviously
connected to the original code.}

\item{debug_enable}{Enable debugging commands in generated code
(currently \code{print()}). If \code{TRUE} then these are generated by
odin targets that support them, and will generally make your
program slower.}

\item{substitutions}{Optionally, a list of values to substitute
into model specification as constants, even though they are
declared as \code{user()}. This will be most useful in conjunction
with \code{rewrite_dims} to create a copy of your model with
dimensions known at compile time and all loops using literal
integers.}

\item{options}{Named list of options.  If provided, then all other
options are ignored.}
}
\value{
A list of parameters, of class \code{odin_options}
}
\description{
For lower-level odin functions \link{odin_parse},
\link{odin_validate} we only accept a list of options rather
than individually named options.
}
\examples{
odin_options()
}
