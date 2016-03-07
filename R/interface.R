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
          self$init <- .Call(self$C$init, self$ptr, 0.0)
        }
        self$order <- .Call(self$C$order, self$ptr)
        self$output_order <- .Call(self$C$output_order, self$ptr)
        self$ode <- if (self$has_delay) deSolve::dede else deSolve::ode
      },

      set_user=function(pars) {
        if (self$has_user) {
          .Call(self$C$set_user, self$ptr, pars)
          if (self$initial_stage == STAGE_USER) {
            init <<- .Call(self$C$init, self$ptr, 0)
          }
          ## TODO: only needs doing if we have arrays with STAGE_USER
          ## dim() calls.
          self$order <- .Call(self$C$order, self$ptr)
          self$output_order <- .Call(self$C$order, self$ptr)
        } else {
          stop("This model does not have parameters")
        }
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
            y <- .Call(self$C$init, self$ptr, t[[0L]])
          }
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
