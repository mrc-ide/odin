on_appveyor <- function() {
  identical(Sys.getenv("APPVEYOR"), "True")
}


on_travis <- function() {
  identical(Sys.getenv("TRAVIS"), "true")
}


on_cran <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true")
}


on_windows <- function() {
  tolower(Sys.info()[["sysname"]]) == "windows"
}


on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI")))
}


skip_on_windows_gha <- function() {
  ## There are mystery issues with finding the odin package being
  ## tested on windows gha
  if (on_ci() && on_windows()) {
    testthat::skip("On Windows Github Actions")
  }
}


validate_ir <- function() {
  ## Not worth the faff, and not expected to fail anyway
  if (on_cran()) {
    FALSE
  }
  ## Not sure why this is failing, or why, but seems related to V8.  I
  ## can't replicate easily, valgrind reports no issues, and it was
  ## introduced with an update to the jsonvalidate package.
  if (on_travis() && getRversion() < numeric_version("3.6.0")) {
    FALSE
  }
  requireNamespace("jsonvalidate", quietly = TRUE)
}


options(odin.verbose = FALSE,
        odin.validate = validate_ir(),
        odin.target = NULL)


unload_dlls <- function() {
  model_cache_clear()
  gc()
}


## access private environment for testing
r6_private <- function(cl) {
  environment(cl$initialize)$private
}


odin_target_name <- function(using = NULL) {
  odin_options(target = using)$target
}


skip_for_target <- function(target, reason = NULL, using = NULL) {
  if (target == odin_target_name(using)) {
    if (is.null(reason)) {
      msg <- sprintf("Engine is %s", target)
    } else {
      msg <- sprintf("Engine is %s (%s)", target, reason)
    }
    testthat::skip(msg)
  }
}


with_options <- function(opts, code) {
  oo <- options(opts)
  on.exit(oo)
  force(code)
}


model_cache_clear <- function() {
  .odin$model_cache_c$clear()
}


## Run a deSolve model
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


test_odin_targets <- function() {
  if (on_cran()) {
    "r"
  } else {
    c("r", "c")
  }
}


## A helper that will run a code block with each target type
test_that_odin <- function(desc, code) {
  testthat::skip_if_not_installed("rlang")
  targets <- test_odin_targets()
  code_enq <- rlang::enquo(code)
  for (target in targets) {
    opts <- list(odin.target = target,
                 odin.rewrite_constants = target == "c")
    testthat::test_that(sprintf("%s (%s)", desc, target),
                        withr::with_options(opts, rlang::eval_tidy(code_enq)))
  }
}
