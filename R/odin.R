##' Create an odin model from a file, text string(s) or expression.
##' The `odin_` version is a "standard evaluation" escape hatch.
##'
##' **Do not use `odin::odin` in a package; you almost certainly want
##' to use [odin::odin_package] instead.**
##'
##' A generated model can return information about itself;
##' [odin::odin_ir]
##'
##' @section User parameters:
##'
##' If the model accepts user parameters, then the parameter to the
##'   constructor or the `$set_user()` method can be used to control
##'   the behaviour when unknown user actions are passed into the
##'   model. Possible values are the strings `stop` (throw an error),
##'   `warning` (issue a warning but keep going), `message` (print a
##'   message and keep going) or `ignore` (do nothing).  Defaults to
##'   the option `odin.unused_user_action`, or `warning` otherwise.
##'
##' @section Delay equations with dde:
##'
##' When generating a model one must chose between using the
##' `dde` package to solve the system or the default
##' `deSolve`.  Future versions may allow this to switch when
##' using `run`, but for now this requires tweaking the generated
##' code to a point where one must decide at generation.  `dde`
##' implements only the Dormand-Prince 5th order dense output solver,
##' with a delay equation solver that may perform better than the
##' solvers in deSolve.  For non-delay equations, `deSolve` is
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
##'   `odin.verbose` or `FALSE` otherwise.
##'
##' @param target Compilation target.  Options are "c" and "r",
##'   defaulting to the option `odin.target` or "c" otherwise.
##'
##' @param workdir Directory to use for any generated files.  This is
##'   only relevant for the "c" target.  Defaults to the value of the
##'   option `odin.workdir` or [tempdir()] otherwise.
##'
##' @param validate Validate the model's intermediate representation
##'   against the included schema.  Normally this is not needed and is
##'   intended primarily for development use.  Defaults to the value
##'   of the option `odin.validate` or `FALSE` otherwise.
##'
##' @param pretty Pretty-print the model's intermediate
##'   representation.  Normally this is not needed and is intended
##'   primarily for development use.  Defaults to the value of the
##'   option `odin.pretty` or `FALSE` otherwise.
##'
##' @param skip_cache Skip odin's cache.  This might be useful if the
##'   model appears not to compile when you would expect it to.
##'   Hopefully this will not be needed often.  Defaults to the option
##'   `odin.skip_cache` or `FALSE` otherwise.
##'
##' @param compiler_warnings Previously this attempted detection of
##'   compiler warnings (with some degree of success), but is
##'   currently ignored. This may become supported again in a future
##'   version depending on underlying support in pkgbuild.
##'
##' @param no_check_unused_equations If `TRUE`, then don't print
##'   messages about unused variables.  Defaults to the option
##'   `odin.no_check_unused_equations` or `FALSE` otherwise.
##'
##' @param options Named list of options.  If provided, then all other
##'   options are ignored.
##'
##' @return An `odin_generator` object (an R6 class) which can be used
##'   to create model instances.
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
##' mod <- exp_decay$new()
##'
##' ## Run the model for a series of times from 0 to 10:
##' t <- seq(0, 10, length.out = 101)
##' y <- mod$run(t)
##' plot(y, xlab = "Time", ylab = "y", main = "", las = 1)
odin <- function(x, verbose = NULL, target = NULL, workdir = NULL,
                 validate = NULL, pretty = NULL, skip_cache = NULL,
                 compiler_warnings = NULL, no_check_unused_equations = NULL,
                 options = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin_(xx, verbose, target, workdir, validate, pretty, skip_cache,
        compiler_warnings, no_check_unused_equations, options)
}


##' @export
##' @rdname odin
odin_ <- function(x, verbose = NULL, target = NULL, workdir = NULL,
                  validate = NULL, pretty = NULL, skip_cache = NULL,
                  compiler_warnings = NULL, no_check_unused_equations = NULL,
                  options = NULL) {
  options <- odin_options(
    verbose = verbose,
    target = target,
    workdir = workdir,
    validate = validate,
    pretty = pretty,
    skip_cache = skip_cache,
    no_check_unused_equations = no_check_unused_equations,
    compiler_warnings = compiler_warnings,
    options = options)

  ir <- odin_parse_(x, options)
  odin_generate(ir, options)
}


odin_generate <- function(ir, options) {
  odin_message(paste("Generating model in", options$target), options$verbose)
  switch(options$target,
         "r" = odin_r_wrapper(ir, options),
         "c" = odin_c_wrapper(ir, options),
         stop(sprintf("Unknown target '%s'", options$target)))
}
