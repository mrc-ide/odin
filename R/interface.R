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
##' @title Create an odin model
##' @param x Either the name of a file to read, a text string (if
##'   length is greater than 1 elements will be joined with newlines)
##'   or an expression.
##' @param dest Destination \emph{directory} for generated files.  The
##'   default is the current directory, but \code{tempdir()} is
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
##' }, dest=tempdir())
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
##' }, dest=tempdir(), build=FALSE)
##'
##' ## Lots of code:
##' cat(paste0(readLines(path), "\n"))
odin <- function(x, dest=".", build=TRUE, load=TRUE, verbose=TRUE) {
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
      stop("'x' looks like a file, but file does not exist")
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
      ret <- ode_system_generator(dll)
    }
  }

  ret
}

##' Test if compilation appears possible.  This is used in some
##' examples, and tries compiling a trivial C program with \code{R CMD
##' SHLIB}.  Results are cached between runs within a session so this
##' should be fast to rely on.
##' @title Test if compilation is possible
##' @return A logical scalar
##' @export
##' @examples
##' can_compile()
can_compile <- function() {
  if (getOption("odin.can_compile", FALSE)) {
    tmp <- tempfile()
    dir.create(tmp)
    owd <- setwd(tmp)
    on.exit({
      setwd(owd)
      unlink(tmp, recursive=TRUE)
    })
    file <- system.file("hello.c", package="odin", mustWork=TRUE)
    Sys.setenv(R_TESTS="")
    code <- system2(file.path(R.home(), "bin", "R"),
                    c("CMD", "SHLIB", file),
                    stdout=FALSE, stderr=FALSE)
    options(odin.can_compile=code == 0L)
  }
  getOption("odin.can.compile", FALSE)
}

## Generate an interface.  This is a bit tricky as we want to generate
## very slightly different interfaces based on the options in info.
## For now this is done sub-optimally but I think it's fine for now at
## least.
##
## TODO: Consider an option here to return either the generator or the
## function that tries to collect the variables up and skip $new.
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
      user=info$user,
      initial_stage=info$initial_stage,

      ## More volitile:
      ptr=NULL,
      ode=NULL,

      ## Cache:
      init=NULL,
      order=NULL,
      output_order=NULL,
      transform_variables=NULL,
      collect_user=NULL,

      ## TODO: both initialize and set_user should optionally be fully
      ## generated to take a proper argument lists derived from
      ## info$user.
      initialize=function(user=NULL) {
        "odin"
        self$ptr <- .Call(self$C$create, user)
        if (self$initial_stage < STAGE_TIME) {
          ## NOTE: Because we never use time in this case it's safe to
          ## pass anything at all through here.
          self$init <- .Call(self$C$init, self$ptr, NA_real_)
        }
        self$update_cache()
        self$ode <- if (self$has_delay) deSolve::dede else deSolve::ode
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
          ## TODO: only needs doing if we have arrays with STAGE_USER
          ## dim() calls.
          self$update_cache()
        } else {
          stop("This model does not have parameters")
        }
        invisible(self$init)
      },

      update_cache=function() {
        self$order <- .Call(self$C$order, self$ptr)
        self$output_order <- .Call(self$C$output_order, self$ptr)
        self$transform_variables <- make_transform_variables(self)
      },

      deriv=function(t, y) {
        if (self$has_delay) {
          stop("deriv() is not supported in delay models")
        }
        .Call(self$C$deriv, self$ptr, t, y)
      },

      run=function(t, y=NULL, ...) {
        if (is.null(y)) {
          if (self$initial_stage < STAGE_TIME) {
            y <- self$init
          } else {
            y <- .Call(self$C$init, self$ptr, t[[1L]])
          }
        } else if (self$initial_stage == STAGE_TIME) {
          ## TODO: this is going to initialise the correct starting
          ## time but also compute and return the y values, which we
          ## don't need.
          .Call(self$C$init, self$ptr, t[[1L]])
        }

        self$ode(y, t, self$C$ds_deriv, self$ptr,
                 initfunc=self$C$ds_initmod, dllname=self$dll,
                 nout=sum(self$output_order), ...)
      },

      contents=function() {
        .Call(self$C$contents, self$ptr)
      }
    ))

  make_user_collector(info$user, quote(cl$new), environment())
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
    ## deSolve does not support this (yet)
    ds_deriv=sprintf("%s_ds_derivs", name),
    ds_initmod=sprintf("%s_ds_initmod", name))
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
  ## Then we can work out this information about the model:
  order <- x$order
  n <- length(x$order)
  len <- vnapply(order, function(x) if (is.null(x)) 1L else prod(x),
                 USE.NAMES=FALSE)
  i1 <- cumsum(len)
  i0 <- c(1L, i1[-n] + 1L)

  tot <- sum(len)
  is_scalar <- vlapply(order, is.null)
  is_array <- !is_scalar

  ## We can detect time except for the cases where the output length
  ## is one.  But we should be able to work with this by checking for
  ## the length being equal to tot, at least for now, but this does
  ## need dealing with.
  function(y) {
    if (is.matrix(y)) {
      has_time <- ncol(y) > tot
      ret <- setNames(vector("list", n), names(order))
      ## Here, it might make sense to treat length1 arrays as scalars, but
      ## that might complicarte things if there are user sized arrays in the
      ## way that sapply breaks everything.
      if (any(is_scalar)) {
        ret[is_scalar] <- lapply(which(is_scalar) + has_time,
                                 function(i) y[, i])
      }
      if (any(is_array)) {
        nr <- nrow(y)
        ret[is_array] <- lapply(which(is_array), function(i)
          array(y[, i0[[i]]:i1[[i]] + has_time], c(nr, order[[i]])))
      }
    } else {
      has_time <- length(y) > tot
      ret <- setNames(vector("list", n), names(order))
      if (any(is_scalar)) {
        ret[is_scalar] <- y[which(is_scalar) + has_time]
      }
      if (any(is_array)) {
        ret[is_array] <- lapply(which(is_array), function(i)
          array(y[i0[[i]]:i1[[i]] + has_time], order[[i]]))
      }
    }
    ret
  }
}

make_user_collector <- function(user, call, env) {
  if (is.null(user)) {
    args <- list(bquote(.(call)(NULL)))
  } else {
    req <- names(user)[user]
    opt <- names(user)[!user]
    collect <- as.call(c(list(quote(list)),
                         setNames(lapply(req, as.symbol), req),
                         setNames(lapply(opt, as.symbol), opt)))
    args <- c(setNames(rep(alist(user=), length(req)), req),
              setNames(rep(alist(user=NULL), length(opt)), opt),
              list(user=collect),
              ## Body:
              bquote(.(call)(user)))
  }
  as.function(args, env)
}