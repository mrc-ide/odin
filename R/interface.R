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
##' t <- seq(0, 10, length.out=101)
##' y <- mod$run(t)
##' plot(y, xlab="Time", ylab="y", main="", las=1)
##'
##' }
##'
##' ## If you want to see the underlying C code, pass build=FALSE:
##' ## (Note: this step does not require a working C compiler)
##' path <- odin::odin({
##'   deriv(y) <- -0.5 * y
##'   initial(y) <- 1
##' }, build=FALSE)
##'
##' ## Lots of code:
##' cat(paste0(readLines(path), "\n"))
odin <- function(x, dest = NULL, build = TRUE, verbose = TRUE,
                 compiler_warnings = NULL) {
  ## TODO: It might be worth adding a check for missing-ness here in
  ## order to generate a sensible error message?
  ##
  ## TODO: It might be worth clarifying that build = FALSE is only used
  ## for testing.
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  }
  odin_(xx, dest, build, verbose, compiler_warnings)
}

##' @export
##' @rdname odin
odin_ <- function(x, dest = NULL, build = TRUE, verbose = TRUE,
                  compiler_warnings = NULL) {
  if (is.null(dest)) {
    dest <- tempfile("odin_")
  }

  ## Generate (or retrieve) the C source code
  ##
  ## Add key versions (or hash of such) into the model as a comment.
  ## This is easy to do.  That will guarantee that we invalidate the
  ## C->DLL cache!
  model <- model_cache_get(hash_model(x))
  if (!is.null(model)) {
    if (verbose) {
      message("Using cached model")
    }
    return(model$model)
  }

  dat <- odin_parse(x)
  code_c <- odin_generate(dat, package = FALSE)
  code_r <- paste(odin_generate_r(dat$info, DLL_PLACEHOLDER), collapse = "\n")

  ## TODO: factor this out elsewhere and test it.
  if (is_directory(dest) || !grepl(".", basename(dest), fixed = TRUE)) {
    if (is.null(dat$file)) {
      base <- dat$config$base
    } else {
      base <- basename_no_ext(dat$file)
    }
    path_c <- file.path(dest, paste0(base, ".c"))
  } else {
    path_c <- dest
    dest <- dirname(path_c)
  }
  dir.create(dest, FALSE, TRUE)

  if (file.exists(path_c)) {
    txt <- readLines(path_c, 5L)
    if (txt[[1]] != "// This file was automatically generated by odin.") {
      stop("Refusing to overwrite non-odin file ", path_c)
    }
    cmp <- sub("// hash: ", "", txt[[5L]], fixed = TRUE)
    if (cmp != dat$hash$inputs) {
      writeLines(code_c, path_c)
    }
  } else {
    writeLines(code_c, path_c)
  }

  if (!build) {
    return(path_c)
  }

  dll <- compile(path_c, verbose = verbose,
                 compiler_warnings = compiler_warnings)
  dyn_load(dll$dll)

  code_r_full <- gsub(DLL_PLACEHOLDER, dll$base, code_r)
  model <- eval(parse(text = code_r_full, keep.source = FALSE), environment())

  model_cache_put(dat$hash, model, dll)
  model
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
can_compile <- function(verbose=FALSE, skip_cache=FALSE) {
  ## should offer a verbose option
  ## should check if GCC is in the path?
  if (skip_cache || is.null(getOption("odin.can_compile"))) {
    tmp <- tempfile()
    dir.create(tmp)
    owd <- setwd(tmp)
    on.exit({
      setwd(owd)
      unlink(tmp, recursive=TRUE)
    })
    file <- "hello.c"
    writeLines("#include <R.h>", file)
    dest <- if (verbose) "" else FALSE
    Sys.setenv(R_TESTS="")
    code <- system2(file.path(R.home(), "bin", "R"),
                    c("CMD", "SHLIB", "hello.c"),
                    stdout=dest, stderr=dest)
    if (code != 0L && Sys.which("gcc") == "") {
      message("I don't see gcc on the PATH") # nocov
    }
    options(odin.can_compile=code == 0L)
  }
  getOption("odin.can_compile", FALSE)
}

odin_prepare <- function(obj, discrete) {
  if (!is.null(obj$output_order)) {
    obj$output_length <- as.integer(
      sum(vnapply(obj$output_order,
                  function(x) if (is.null(x)) 1 else prod(x))))
  }
  obj$names <- make_names(c(obj$variable_order, obj$output_order), discrete)
  obj$transform_variables <- make_transform_variables(obj)
  obj
}

## At the same time, things like row-wise access of the list based
## form is basically impossible so we should consider classing this
## and retaining the original output so that x[, 1] on the output runs
## through the transformation.  But for x[, 1:5] and logical indices?
## Who knows?  This is _hard_...
make_transform_variables <- function(x) {
  ord <- c(setNames(list(NULL), TIME), x$variable_order, x$output_order)
  j <- seq_along(x$variable_order)
  n <- length(ord)
  len <- vnapply(ord, function(x) if (is.null(x)) 1L else prod(x),
                 USE.NAMES=FALSE)
  i1 <- cumsum(len)
  i0 <- c(1L, i1[-n] + 1L)
  tot <- sum(len)
  is_scalar <- vlapply(ord, is.null)
  is_array <- !is_scalar
  nms <- names(ord)

  ## We can detect time except for the cases where the output length
  ## is one.  But we should be able to work with this by checking for
  ## the length being equal to tot, at least for now, but this does
  ## need dealing with.

  ## But with deSolve output, the output variables are always included
  ## -- it's only in the case where the derivatives are being
  ## calculated (where the output is an attribute) that we care
  ## otherwise.  In that case there is no time variable either.
  function(y) {
    ny <- if (is.matrix(y)) ncol(y) else length(y)
    has_time <- ny == tot
    if (!has_time) {
      if (ny != tot - 1L) {
        stop("Unexpected size input")
      }
      i0 <- i0 - 1L
      i1 <- i1 - 1L
      i0[1L] <- NA_integer_
      i1[1L] <- NA_integer_
    }

    ret <- setNames(vector("list", n), nms)

    if (is.matrix(y)) {
      ## Here, it might make sense to treat length1 arrays as scalars,
      ## but that might complicate things in the way that sapply does.
      ## Probably length1 arrays should be kept as arrays...
      if (any(is_scalar)) {
        ret[is_scalar] <- lapply(i0[is_scalar], function(i) y[, i])
      }
      if (any(is_array)) {
        nr <- nrow(y)
        ret[is_array] <- lapply(which(is_array), function(i)
          array(y[, i0[[i]]:i1[[i]]], c(nr, ord[[i]])))
      }
    } else {
      if (any(is_scalar)) {
        ret[is_scalar] <- y[i0[is_scalar]]
      }
      if (any(is_array)) {
        shape_array <- function(x, ord) {
          if (length(ord) == 1L) unname(x) else array(x, ord)
        }
        ret[is_array] <- lapply(which(is_array), function(i)
          shape_array(y[i0[[i]]:i1[[i]]], ord[[i]]))
      }
    }
    ret
  }
}

make_names <- function(ord, discrete = FALSE) {
  if (all(vlapply(ord, is.null))) {
    nms <- names(ord)
  } else {
    f <- function(x) {
      if (is.null(x)) {
        ""
      } else {
        sprintf("[%s]", apply(expand_grid_int(x), 1, paste, collapse=","))
      }
    }
    idx <- unname(lapply(ord, f))
    nms <- sprintf("%s%s", rep(names(ord), lengths(idx)), unlist(idx))
  }
  c(if (discrete) STEP else TIME, nms)
}
