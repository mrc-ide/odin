run_model <- function(model, times, parms = NULL, ...) {
  y <- model$initial(times[[1L]], parms)
  if (isTRUE(model$delay)) {
    ## TODO: in theory, this will not work correctly with rk4 & friends
    lags <- list(mxhist = 10000)
  } else {
    lags <- NULL
  }
  ## TODO: I'm not actually certain that this is the best way of
  ## passing parameters.  We might need to step through deSolve's ODE
  ## initialisation here, but I'm not sure.  I think that this
  ## approach here will be a touch more general, but some additional
  ## work might be needed to deal with globals and the possibilities
  ## of nested models; I'll probably handle that with a pointer
  ## though.
  deSolve::ode(y, times, model$derivs, NULL, lags = lags, ...)
}
