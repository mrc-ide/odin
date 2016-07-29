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
##' @param x Either the name of a file to read, a text string (if
##'   length is greater than 1 elements will be joined with newlines)
##'   or an expression.
##' @param dest Destination \emph{directory} for generated files.  The
##'   default is the temporary directory, but \code{"."} is
##'   another useful value.
##' @param build Logical scalar indicating if we should build the
##'   model (i.e. compile the dll).
##' @param load Logical scalar indicating if the dll should be loaded
##'   and an \code{ode_generator} object returned.  Only used if
##'   \code{build} is \code{TRUE}.
##' @param verbose Logical scalar indicating if the compilation should
##'   be verbose.  In future versions this may also make the
##'   parse/generate step be verbose too.
##' @return If \code{load} is \code{TRUE}, an \code{ode_generator}
##'   object, otherwise the filename of the generated C file.
##' @author Rich FitzJohn
##' @export
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
odin <- function(x, dest=tempdir(), build=TRUE, load=TRUE, verbose=TRUE) {
  ## TODO: It might be worth adding a check for missing-ness here in
  ## order to generate a sensible error message?
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  }
  odin_(xx, dest, build, load, verbose)
}

##' @export
##' @rdname odin
odin_ <- function(x, dest=".", build=TRUE, load=TRUE, verbose=TRUE) {
  if (is.language(x)) {
    as <- "expression"
  } else if (is.character(x)) {
    ## We're really looking for a separator given that we need
    if (length(x) > 1L) {
      as <- "text"
      x <- paste(x, collapse="\n")
    } else if (grepl("[\n;]", x)) {
      as <- "text"
    } else if (file.exists(x)) {
      as <- "file"
    } else {
      stop("'x' looks like a filename, but file does not exist")
    }
  } else {
    stop("Invalid type for 'x'")
  }

  dat <- odin_parse(x, as)
  path <- odin_generate(dat, dest)
  ret <- path
  if (build) {
    dll <- compile(path, verbose, load)
    if (load) {
      ret <- ode_system_generator(dll, dat$config$base)
    }
  }

  ret
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
      message("I don't see gcc on the PATH")
    }
    options(odin.can_compile=code == 0L)
  }
  getOption("odin.can_compile", FALSE)
}

## Generate an interface.  This is a bit tricky as we want to generate
## very slightly different interfaces based on the options in info.
## For now this is done sub-optimally but I think it's fine for now at
## least.
##
## TODO: Consider an option here to return either the generator or the
## function that tries to collect the variables up and skip $new.
##
## TODO: Do we also want something that will act as "destroy" and
## unload the DLL and void all the pointers?  That requires that we
## keep a pointer cache here, but that's easy enough.  We can register
## this for eventual garbage collection too, so that's nice.

##' Generate an ODE interface from an odin shared library.  This is
##' used internally when generating packages, and should not be used
##' by end users.
##' @title Generate ODE system
##' @param dll Name of the shared library (with no extension)
##' @param name Name of the model within the shared library
##' @keywords internal
##' @export
ode_system_generator <- function(dll, name=NULL) {
  self <- NULL # for R CMD check
  ## At present this is not going to work well for constructing custom
  ## initialisers but we can get there eventually.
  if (is.null(name)) {
    name <- basename_no_ext(dll)
  }
  info <- .Call(paste0(name, "_info"), PACKAGE=dll)
  cl <- R6::R6Class(
    "ode_system",
    public=list(

      ## Bunch of stored stuff:
      name=name,
      dll=dll,
      C=odin_dll_info(name, dll),
      has_delay=info$has_delay,
      has_user=length(info$user) > 0L,
      has_output=info$has_output,
      has_interpolate=info$has_interpolate,
      ## TODO: make this configurable as info$use_dde
      use_dde=FALSE,
      user=info$user,
      initial_stage=info$initial_stage,
      dim_stage=info$dim_stage,
      interpolate_t=NULL,

      ## More volitile:
      ptr=NULL,
      ode=NULL,

      ## Cache:
      init=NULL,
      order=NULL,
      output_order=NULL,
      transform_variables=NULL,
      output_length=NULL,

      ## TODO: both initialize and set_user should optionally be fully
      ## generated to take a proper argument lists derived from
      ## info$user.
      initialize=function(user=NULL, dde=FALSE) {
        ## TODO: check that dde is a non-missing logical scalar
        self$use_dde <- dde
        self$ptr <- .Call(self$C$create, user, self$use_dde)
        if (self$initial_stage < STAGE_TIME) {
          ## NOTE: Because we never use time in this case it's safe to
          ## pass anything at all through here.
          self$init <- .Call(self$C$init, self$ptr, NA_real_)
        }
        self$update_cache()
        if (self$use_dde) {
          loadNamespace("dde")
          self$ode <- dde::dopri5
        } else if (self$has_delay) {
          self$ode <- deSolve::dede
        } else {
          self$ode <- deSolve::ode
        }
      },

      ## This function is problematic because it's fairly hard to
      ## check the arguments safely.  I want to run the arguments
      ## through a sanitiser first.
      set_user=function(..., user=list(...)) {
        if (self$has_user) {
          .Call(self$C$set_user, self$ptr, user)
          if (self$initial_stage == STAGE_USER) {
            self$init <- .Call(self$C$init, self$ptr, NA_real_)
          }
          self$dim_stage == STAGE_USER
        } else {
          stop("This model does not have parameters")
        }
        invisible(self$init)
      },

      update_cache=function() {
        self$order <- .Call(self$C$order, self$ptr)
        self$output_order <- .Call(self$C$output_order, self$ptr)
        self$output_length <- as.integer(
          sum(vnapply(self$output_order, function(x)
            if (is.null(x)) 1 else prod(x))))
        self$transform_variables <- make_transform_variables(self)
        if (self$has_interpolate) {
          self$interpolate_t <- .Call(self$C$interpolate_t, self$ptr)
        }
      },

      deriv=function(t, y) {
        if (self$has_delay) {
          stop("deriv() is not supported in delay models")
        }
        .Call(self$C$deriv, self$ptr, t, y)
      },

      initial=function(t0) {
        if (self$initial_stage < STAGE_TIME) {
          self$init
        } else {
          ## TODO: is length checked before dereferencing in C?
          .Call(self$C$init, self$ptr, as.numeric(t0))
        }
      },

      run=function(t, y=NULL, ...) {
        t0 <- t[[1L]]
        if (is.null(y)) {
          y <- self$initial(t0)
        } else if (self$initial_stage == STAGE_TIME) {
          ## TODO: this is going to initialise the correct starting
          ## time but also compute and return the y values, which we
          ## don't need.  Would be nicer to have a set_time target we
          ## can hit.
          .Call(self$C$init, self$ptr, as.numeric(t0))
        }
        if (self$has_interpolate) {
          r <- self$interpolate_t
          if (t0 < r[[1L]]) {
            stop("Integration times do not span interpolation range; min: ",
                 r[[1L]])
          }
          if (!is.na(r[[2L]]) && t[[length(t)]] > r[[2L]]) {
            stop("Integration times do not span interpolation range; max: ",
                 r[[2L]])
          }
        }
        if (self$use_dde) {
          ## TODO: this doesn't allow for n_history to be tweaked by
          ## the calling function, which would be useful if we want to
          ## exploit the history.  That also requires passing in
          ## keep_history=TRUE.
          n_history <- if (self$has_delay) 1000L else 0L
          ## NOTE: This is a bit shit, but does the job for now:
          n_out <- self$output_length
          output <- if (n_out > 0L) self$C$dde_output else NULL
          ret <- self$ode(y, t, self$C$dde_deriv, self$ptr,
                          dllname=self$dll, n_out=n_out, output=output,
                          n_history=n_history, keep_history=FALSE,
                          parms_are_real=FALSE,
                          ## Try and preserve some compatibility with deSolve:
                          by_column=TRUE, keep_initial=TRUE,
                          ...)
          cbind(t, ret, attr(ret, "output"), deparse.level=0)
        } else {
          self$ode(y, t, self$C$ds_deriv, self$ptr,
                   initfunc=self$C$ds_initmod, dllname=self$dll,
                   nout=sum(self$output_order), ...)
        }
      },

      contents=function() {
        .Call(self$C$contents, self$ptr)
      }
    ))

  make_user_collector(info$user, quote(cl$new), environment(), FALSE)
}

odin_dll_info <- function(name, dll) {
  ## TODO: Something indicating how many parameters we are willing to
  ## take for the initialisation part.
  ret <- list(
    create=getNativeSymbolInfo(sprintf("%s_create", name), dll),
    init=getNativeSymbolInfo(sprintf("%s_initialise", name), dll),
    set_user=getNativeSymbolInfo(sprintf("r_%s_set_user", name), dll),
    deriv=getNativeSymbolInfo(sprintf("r_%s_deriv", name), dll),
    contents=getNativeSymbolInfo(sprintf("%s_contents", name), dll),
    order=getNativeSymbolInfo(sprintf("%s_order", name), dll),
    output_order=getNativeSymbolInfo(sprintf("%s_output_order", name), dll),
    interpolate_t=getNativeSymbolInfo(sprintf("%s_interpolate_t", name), dll),
    ## deSolve does not support this (yet)
    ds_deriv=sprintf("%s_ds_derivs", name),
    ds_initmod=sprintf("%s_ds_initmod", name),
    dde_deriv=sprintf("%s_dde_derivs", name),
    dde_output=sprintf("%s_dde_output", name))
}

## TODO: depending on the dimensionality stage we will want to run
## this at other times; that needs to be added to the interface bits
## as soon as variable sized arrays are handled (I have most of the
## ingredients for this now, I think).

## The issue here is that in the case of purely scalar variables we
## really want to return a matrix not a vector.  Matrices are heaps
## faster dto deal with (especially row-wise access) so we should make
## that an option I think.

## At the same time, things like row-wise access of the list based
## form is basically impossible so we should consider classing this
## and retaining the original output so that x[, 1] on the output runs
## through the transformation.  But for x[, 1:5] and logical indices?
## Who knows?  This is _hard_...
make_transform_variables <- function(x) {
  ord <- c(x$order, x$output_order)
  j <- seq_along(x$order)
  n <- length(ord)
  len <- vnapply(ord, function(x) if (is.null(x)) 1L else prod(x),
                   USE.NAMES=FALSE)
  i1 <- cumsum(len)
  i0 <- c(1L, i1[-n] + 1L)
  tot <- sum(len)
  is_scalar <- vlapply(ord, is.null)
  nms <- names(ord)

  tot_vars <- sum(len)
  n_vars <- length(x$order)

  ## We can detect time except for the cases where the output length
  ## is one.  But we should be able to work with this by checking for
  ## the length being equal to tot, at least for now, but this does
  ## need dealing with.

  ## But with deSolve output, the output variables are always included
  ## -- it's only in the case where the derivatives are being
  ## calculated (where the output is an attribute) that we care
  ## otherwise.  In that case there is no time variable either.
  function(y) {
    if (is.matrix(y)) {
      has_output <- ncol(y) > tot_vars
      has_time <- ncol(y) > tot
      j <- if (has_output) seq_len(n) else seq_len(n_vars)

      is_scalar <- is_scalar[j]
      is_array <- !is_scalar
      ret <- setNames(vector("list", n), nms[j])

      ## Here, it might make sense to treat length1 arrays as scalars,
      ## but that might complicate things in the way that sapply does.
      ## Probably length1 arrays should be kept as arrays...
      if (any(is_scalar)) {
        ret[is_scalar] <- lapply(which(is_scalar) + has_time,
                                 function(i) y[, i])
      }
      if (any(is_array)) {
        nr <- nrow(y)
        ret[is_array] <- lapply(which(is_array), function(i)
          array(y[, i0[[i]]:i1[[i]] + has_time], c(nr, ord[[i]])))
      }
    } else {
      has_output <- length(y) > tot_vars
      has_time <- length(y) > tot
      j <- if (has_output) seq_len(n) else seq_len(n_vars)

      is_scalar <- is_scalar[j]
      is_array <- !is_scalar
      ret <- setNames(vector("list", n), nms[j])

      if (any(is_scalar)) {
        ret[is_scalar] <- y[which(is_scalar) + has_time]
      }
      if (any(is_array)) {
        ret[is_array] <- lapply(which(is_array), function(i)
          array(y[i0[[i]]:i1[[i]] + has_time], ord[[i]]))
      }
    }
    ret
  }
}

make_user_collector <- function(user, call, env, dde) {
  if (is.null(user)) {
    args <- c(list(dde=dde),
              bquote(.(call)(NULL, dde)))
  } else {
    req <- names(user)[user]
    opt <- names(user)[!user]
    collect <- as.call(c(list(quote(list)),
                         setNames(lapply(req, as.symbol), req),
                         setNames(lapply(opt, as.symbol), opt)))
    args <- c(setNames(rep(alist(user=), length(req)), req),
              setNames(rep(alist(user=NULL), length(opt)), opt),
              list(user=collect, dde=dde),
              ## Body:
              bquote(.(call)(user, dde)))
  }
  as.function(args, env)
}
