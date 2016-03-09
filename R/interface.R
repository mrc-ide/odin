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
##' mod <- exp_decay$new()
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
      browser()
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

can_compile_result <- NULL

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
  if (is.null(can_compile_result)) {
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
    can_compile_result <<- code == 0L
  }
  can_compile_result
}

## Generate an interface.  This is a bit tricky as we want to generate
## very slightly different interfaces based on the options in info.
## For now this is done sub-optimally but I think it's fine for now at
## least.
ode_system_generator <- function(dll, name=NULL) {
  ## At present this is not going to work well for constructing custom
  ## initialisers but we can get there eventually.
  if (is.null(name)) {
    name <- basename_no_ext(dll)
  }
  info <- .Call(paste0(name, "_info"), PACKAGE=dll)
  R6::R6Class(
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

      ## TODO: both initialize and set_user should optionally be fully
      ## generated to take a proper argument lists derived from
      ## info$user.
      initialize=function(pars=NULL) {
        "odin"
        self$ptr <- .Call(self$C$create, pars)
        if (self$initial_stage < STAGE_TIME) {
          ## NOTE: Because we never use time in this case it's safe to
          ## pass anything at all through here.
          self$init <- .Call(self$C$init, self$ptr, NA_real_)
        }
        self$order <- .Call(self$C$order, self$ptr)
        self$output_order <- .Call(self$C$output_order, self$ptr)
        self$ode <- if (self$has_delay) deSolve::dede else deSolve::ode
      },

      set_user=function(pars) {
        if (self$has_user) {
          .Call(self$C$set_user, self$ptr, pars)
          if (self$initial_stage == STAGE_USER) {
            self$init <- .Call(self$C$init, self$ptr, NA_real_)
          }
          ## TODO: only needs doing if we have arrays with STAGE_USER
          ## dim() calls.
          self$order <- .Call(self$C$order, self$ptr)
          self$output_order <- .Call(self$C$order, self$ptr)
        } else {
          stop("This model does not have parameters")
        }
        invisible(self$init)
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
