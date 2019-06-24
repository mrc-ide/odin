##' Create an odin model from a file, text string(s) or expression.
##' The \code{odin_} version is a "standard evaluation" escape hatch.
##'
##' \emph{Do not use \code{odin::odin} in a package; you almost
##' certainly want to use \code{\link{odin_package}} instead; see the
##' \code{odin_package} vignette for more information.}
##'
##' A generated model can return information about itself;
##' \code{\link{odin_ir}}
##'
##' @section User parameters:
##'
##' If the model accepts user parameters, then the parameter to the
##' constructor or the \code{set_user} method can be used to control
##' the behaviour when unknown user actions are passed into the
##' model.Possible values are the strings \code{stop} (throw an
##' error), \code{warning} (issue a warning but keep going),
##' \code{message} (print a message and keep going) or \code{ignore}
##' (do nothing).  Defaults to the option
##' \code{odin.unused_user_action}, or \code{warning} otherwise.  The
##' default behaviour prior to odin version 0.2.0 was equivalent to
##' \code{ignore}.
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
##' @param verbose Logical scalar indicating if the compilation should
##'   be verbose.  Defaults to the value of the option
##'   \code{odin.verbose} or \code{FALSE} otherwise.
##'
##' @param target Compilation target.  Options are "c" and "r",
##'   defaulting to the option \code{odin.target} or "c" otherwise.
##'
##' @param workdir Directory to use for any generated files.  This is
##'   only relevant for the "c" target.  Defaults to the value of the
##'   option \code{odin.workdir} or \code{tempdir()} otherwise.
##'
##' @param validate Validate the model's intermediate representation
##'   against the included schema.  Normally this is not needed and is
##'   intended primarily for development use.  Defaults to the value
##'   of the option \code{odin.validate} or \code{FALSE} otherwise.
##'
##' @param pretty Pretty-print the model's intermediate
##'   representation.  Normally this is not needed and is intended
##'   primarily for development use.  Defaults to the value of the
##'   option \code{odin.pretty} or \code{FALSE} otherwise.
##'
##' @param skip_cache Skip odin's cache.  This might be useful if the
##'   model appears not to compile when you would expect it to.
##'   Hopefully this will not be needed often.  Defaults to the option
##'   \code{odin.skip_cache} or \code{FALSE} otherwise.
##'
##' @param compiler_warnings Logical scalar indicating if compiler
##'   warnings should be converted to R warnings.  If this is
##'   \code{TRUE}, then if any compiler warnings are generated, the
##'   compiler output will be displayed (regardless of the value of
##'   \code{verbose}) within an R warning (suppressible via
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
##'   may not work on all platforms.  Defaults to the option
##'   \code{odin.compiler_warnings} or \code{FALSE} otherwise.
##'
##' @param no_check_unused_equations If \code{TRUE}, then don't print
##'   messages about unused variables.  Defaults to the option
##'   \code{odin.no_check_unused_equations} or \code{FALSE} otherwise.
##'
##' @param no_check_naked_index If \code{TRUE}, then if an index
##'   variable (\code{i}, \code{j}, ...) is used outside of an array
##'   subset (e.g., \code{x[] <- i}) then a notice is printed.  The
##'   behaviour of this functionality changed in odin version
##'   \code{0.2.0} and this flag is intended to notify users about the
##'   change.  See \url{https://github.com/mrc-ide/odin/issues/136}
##'   for more information.  Defaults to the option
##'   \code{odin.no_check_naked_index} or \code{FALSE} otherwise.
##'
##' @return A function that can generate the model
##'
##' @author Rich FitzJohn
##' @export
##' @importFrom R6 R6Class
##' @importFrom deSolve ode dede
##' @importFrom cinterpolate interpolation_function
##' @examples
##' ## Compile the model; exp_decay here is an R6ClassGenerator and will
##' ## generate instances of a model of exponential decay:
##' exp_decay <- odin::odin({
##'   deriv(y) <- -0.5 * y
##'   initial(y) <- 1
##' }, target = "r")
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
odin <- function(x, verbose = NULL, target = NULL, workdir = NULL,
                 validate = NULL, pretty = NULL, skip_cache = NULL,
                 compiler_warnings = NULL, no_check_unused_equations = NULL,
                 no_check_naked_index = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin_(xx, verbose, target, workdir, validate, pretty, skip_cache,
        compiler_warnings, no_check_unused_equations, no_check_naked_index)
}


##' @export
##' @rdname odin
odin_ <- function(x, verbose = NULL, target = NULL, workdir = NULL,
                  validate = NULL, pretty = NULL, skip_cache = NULL,
                  compiler_warnings = NULL, no_check_unused_equations = NULL,
                  no_check_naked_index = NULL) {
  options <- odin_options(verbose = verbose,
                          target = target,
                          workdir = workdir,
                          validate = validate,
                          pretty = pretty,
                          skip_cache = skip_cache,
                          no_check_unused_equations = no_check_unused_equations,
                          no_check_naked_index = no_check_naked_index,
                          compiler_warnings = compiler_warnings)

  ir <- odin_parse_(x, options)
  odin_generate(ir, options)
}


odin_generate <- function(ir, options) {
  dat <- ir_deserialise(ir)
  odin_message(paste("Generating model in", options$target), options$verbose)
  switch(options$target,
         "r" = generate_r(dat, options),
         "c" = generate_c(dat, options),
         stop(sprintf("Unknown target '%s'", options$target)))
}
