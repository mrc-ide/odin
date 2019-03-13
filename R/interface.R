## NOTE: at the moment the strategy is to interrogate the underlying C
## code as to parameters of the model and then use R's reflection to
## build an appropritate class.  There's a bit of slightly awkward
## argument building and this does trigger some NOTEs about use of
## .Call in the code below (as R CMD check's static QA can't resolve
## the symbols for not-yet-generated models).
##
## An alternative would be to generate the R code as well.  That
## avoids some headaches but requires either dependencies on things
## like whisker, more manual code generation and then sourcing the R
## file.

##' Create an odin model from a file, text string(s) or expression.
##' The \code{odin_} version is a "standard evaluation" escape hatch.
##'
##' \emph{Do not use \code{odin::odin} in a package; you almost
##' certainly want to use \code{\link{odin_package}} instead; see the
##' \code{odin_package} vignette for more information.}
##'
##' @section Warning:
##'
##' Be careful with repeated use of \code{odin} on the same code; if
##' you have any models existing and not garbage collected then when
##' the shared library is reloaded this will crash R when the object
##' goes out of scope and is later garbage collected.  I'll tighten
##' this up in future versions, but for now this is an issue.
##'
##' @section Delay equations with dde:
##'
##' When generating a model one must chose between using the
##' \code{dde} package to solve the system or the default
##' \code{deSolve}.  Future versions may allow this to switch when
##' using \code{run}, but for now this requires tweaking the generated
##' code to a point where one must decide at generation.  \code{dde}
##' implements only the Dormand-Prince 5th order dense output solver,
##' with a delay equation solver that may perform better than the
##' solvers in deSolve.  For non-delay equations, \code{deSolve} is
##' very likely to outperform the simple solver implemented.
##'
##' @title Create an odin model
##'
##' @param x Either the name of a file to read, a text string (if
##'   length is greater than 1 elements will be joined with newlines)
##'   or an expression.
##'
##' @param dest Destination \emph{directory} for generated files.  The
##'   default is the temporary directory, but \code{"."} is
##'   another useful value.
##'
##' @param build Logical scalar indicating if we should build the
##'   model (i.e. compile the dll).  Using \code{build = FALSE} is
##'   primarily for internal use, because the model can't be easily
##'   used without some generated R code and that is not returned.
##'
##' @param verbose Logical scalar indicating if the compilation should
##'   be verbose.  In future versions this may also make the
##'   parse/generate step be verbose too.
##'
##' @param compiler_warnings Logical scalar indicating if compiler
##'   warnings should be converted to R warnings.  If this is
##'   \code{TRUE}, then if any compiler warnings are generated, the
##'   compiler output will be displayed (regardless of the value of
##'   \code{verbose}) within an R warning (suppressable via
##'   \code{suppressWarnings} and catchable via \code{tryCatch}).  The
##'   default is to default to \code{FALSE} unless the global option
##'   \code{odin.compiler_warnings} is set to \code{TRUE} (set with
##'   \code{options(odin.compiler_warnings = TRUE)}).  The default may
##'   change to \code{TRUE} in future.  Warnings are currently a mix
##'   of ambiguous syntax in your model (worth fixing) and limitations
##'   in the code that odin generates (which you can't fix but I will
##'   get on to over time).  What is flagged will depend strongly on
##'   your platform and what is in your \code{Makevars}.  I develop
##'   odin with \code{-Wall -Wextra -pedantic} and still see warnings
##'   with both gcc and clang.  The compiler output is very simple and
##'   may not work on all platforms.
##'
##' @param safe Use safe array access; try this option if odin crashes
##'   R in order to track down invalid memory access.  This replaces
##'   all array accesses in C with a version that will check that the
##'   requested element is within bounds.  This will slow down your
##'   model considerably (it checks \emph{every} access, even if it
##'   could be worked out that it would be fine).
##'
##' @param skip_cache Skip odin's cache.  This might be useful if the
##'   model appears not to compile when you would expect it to.
##'   Hopefully this will not be needed often.  Models with \code{safe
##'   = TRUE} will always skip the cache as these are intended for
##'   debugging.
##'
##' @return If \code{build} is \code{TRUE}, an function that can
##'   generate the model, otherwise the filename of the generated C
##'   file.
##'
##' @author Rich FitzJohn
##' @export
##' @importFrom R6 R6Class
##' @importFrom deSolve ode dede
##' @importFrom cinterpolate interpolation_function
##' @examples
##' if (can_compile()) { # only run this if a system is set up to compile
##'
##' ## Compile the model; exp_decay here is an R6ClassGenerator and will
##' ## generate instances of a model of exponential decay:
##' exp_decay <- odin::odin({
##'   deriv(y) <- -0.5 * y
##'   initial(y) <- 1
##' })
##'
##' ## Generate an instance; there are no parameters here so all instances
##' ## are the same and this looks a bit pointless.  But this step is
##' ## required because in general you don't want to have to compile the
##' ## model every time it is used (so the generator will go in a
##' ## package).
##' mod <- exp_decay()
##'
##' ## Run the model for a series of times from 0 to 10:
##' t <- seq(0, 10, length.out = 101)
##' y <- mod$run(t)
##' plot(y, xlab = "Time", ylab = "y", main = "", las = 1)
##'
##' }
##'
##' ## If you want to see the underlying C code, pass build = FALSE:
##' ## (Note: this step does not require a working C compiler)
##' path <- odin::odin({
##'   deriv(y) <- -0.5 * y
##'   initial(y) <- 1
##' }, build = FALSE)
odin <- function(x, dest = NULL, build = TRUE, verbose = TRUE,
                 compiler_warnings = NULL, safe = FALSE, skip_cache = FALSE) {
  stop("Do not use")
}


##' @export
##' @rdname odin
odin_ <- function(x, dest = NULL, build = TRUE, verbose = TRUE,
                  compiler_warnings = NULL, safe = FALSE,
                  skip_cache = FALSE) {
  stop("Do not use")
}


##' Test if compilation appears possible.  This is used in some
##' examples, and tries compiling a trivial C program with \code{R CMD
##' SHLIB}.  Results are cached between runs within a session so this
##' should be fast to rely on.
##'
##' If this function believes you can't compile, and if \code{gcc}
##' can't be found on the path, a diagnostc message will be printed.
##' This will of course not be very interesting if you use a different
##' compiler to gcc!  But the most likely people affected here are
##' Windows users; if you get this ensure that you have rtools
##' installed.  If you have \code{devtools} installed,
##' \code{devtools::find_rtools()} may be helpful for diagnosing
##' compiler issues.
##'
##' @title Test if compilation is possible
##' @param verbose Be verbose when running commands?
##' @param skip_cache Try again to compile, skipping the cached value?
##' @return A logical scalar
##' @export
##' @examples
##' can_compile() # will take ~0.1s the first time
##' can_compile() # should be basically instantaneous
can_compile <- function(verbose = FALSE, skip_cache = FALSE) {
  ## should offer a verbose option
  ## should check if GCC is in the path?
  if (skip_cache || is.null(getOption("odin.can_compile"))) {
    tmp <- tempfile()
    dir.create(tmp)
    owd <- setwd(tmp)
    on.exit({
      setwd(owd)
      unlink(tmp, recursive = TRUE)
    })
    file <- "hello.c"
    writeLines("#include <R.h>", file)
    dest <- if (verbose) "" else FALSE
    Sys.setenv(R_TESTS = "")
    code <- system2(file.path(R.home(), "bin", "R"),
                    c("CMD", "SHLIB", "hello.c"),
                    stdout = dest, stderr = dest)
    if (code != 0L && Sys.which("gcc") == "") {
      message("I don't see gcc on the PATH") # nocov
    }
    options(odin.can_compile = code == 0L)
  }
  getOption("odin.can_compile", FALSE)
}


##' @export
##' @importFrom stats coef
coef.odin_generator <- function(object, ...) {
  attr(object, "user_info")()
}


##' @export
print.odin_generator <- function(x, ...) {
  cat(paste0(format(x, ...), "\n", collapse = ""))
  invisible(x)
}


##' @export
format.odin_generator <- function(x, ...) {
  args <- utils::capture.output(args(x))
  args <- args[-length(args)] # drop 'NULL'
  user_info <- coef(x)
  c(args,
    "<an 'odin_generator' function>",
    if (nrow(user_info) > 0L)
      "  use coef() to get information on user parameters")
}
