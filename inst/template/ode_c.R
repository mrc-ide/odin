{{name}}_ <- R6::R6Class(
  "odin", # hmm, what should this be?
  cloneable = FALSE,

  private = list(
    ptr = NULL,
    use_dde = NULL,

    variable_order = NULL,
    output_order = NULL,
    n_out = NULL,
    ynames = NULL,
    interpolate_t = NULL,

    ## This is never called, but is used to ensure that R finds our
    ## symbols that we will use from the package.
    registration = function() {
      .C("{{name}}_rhs_dde", 1, 2, 3, 4, 5, PACKAGE = "{{package}}")
      .C("{{name}}_rhs_desolve", 1, 2, 3, 4, 5, 6, PACKAGE = "{{package}}")
      .C("{{name}}_initmod_desolve", 1, PACKAGE = "{{package}}")
    },

    update_metadata = function() {
      ## TODO: this all needs a little thought
      meta <- .Call("{{c$metadata}}", private$ptr,
                    PACKAGE = "{{package}}")
      private$variable_order <- meta$variable_order
      private$output_order <- meta$output_order
      private$n_out <- meta$n_out
      ## TODO: this can't be triple colon, pull out as some support
      ## structure? Or we set the environment to be odin's?
      private$ynames <- odin:::make_names(
        private$variable_order, private$output_order, FALSE)
      private$interpolate_t <- meta$interpolate_t
    }
  ),

  public = list(
    initialize = function(..., user = list(...), use_dde = FALSE) {
      private$ptr <- .Call("{{c$create}}", user, PACKAGE = "{{package}}")
      private$use_dde <- use_dde
      private$update_metadata()
    },

    ir = function() {
      ## The IR will always be saved as one line.
      ## TODO: this might get changed?
      path_ir <- system.file("odin/{{name}}.json", mustWork = TRUE,
                             package = "{{package}}")
      json <- readLines(path_ir)
      class(json) <- "json"
      json
    },

    ## Do we need to have the user-settable args here? Probably, but
    ## that's not super straightforward.

    set_user = function(..., user = list(...)) {
      .Call("{{c$set_user}}", private$ptr, user, PACKAGE = "{{package}}")
      private$update_metadata()
    },

    ## This might be time sensitive and, so we can avoid computing
    ## it. I wonder if that's an optimisation we should drop for now
    ## as it does not seem generally useful. This would bring us
    ## closer to the js version which requires that we always pass the
    ## time in.
    initial = function(t) {
      .Call("{{c$initial}}", private$ptr, t,
            PACKAGE = "{{package}}")
    },

    deriv = function(t, y) {
      .Call("{{c$rhs_r}}", private$ptr, t, y, PACKAGE = "{{package}}")
    },

    contents = function() {
      .Call("{{c$contents}}", private$ptr, PACKAGE = "{{package}}")
    },

    transform_variables = function(y) {
      odin:::support_transform_variables(y, private)
    },

    ## Probably the only sane way of dealing with this is a wrapper
    ## function? But the logic here is all relatively cheap
    ##
    ## TODO: n_history should have no effect if has_delay is false,
    ## and should not be passed through to anything. This suggests
    ## that we might be best to write some interface layer here
    ## really, as this is too many options to code generate easily.
    ##
    ## TODO: if we have interpolation we need to check that the time is ok
    run = function(t, y = NULL, ..., use_names = TRUE, tcrit = NULL) {
      t <- as.numeric(t)
      if (is.null(y)) {
        y <- self$initial(t)
      }
      if (private$use_dde) {
        ## TODO: names of the functions should come from the template
        ## data; that will make getting output here easier.
        ##
        ## TODO: Because we might invert or otherwise strip the data
        ## returned here, we should use dde's support for naming, and
        ## check that is actually good enough to do this! (this is out
        ## of scope for the immediate work as it's broken in the
        ## current interface).
        ##
        ## NOTE: it's not obvious how (or if!) we can call into use
        ## the native symbols as found by package registration but it
        ## appears not.
        ret <- dde::dopri(y, t, "{{c$rhs_dde}}", private$ptr,
                          dllname = "{{package}}", parms_are_real = FALSE,
                          n_out = private$n_out, output = {{c$output}},
                          ynames = FALSE, tcrit = tcrit, ...)
      } else {
        ## TODO: if this is a delay function we need to use dde, and
        ## in both cases we need to inject the history length.
        ret <- deSolve::ode(y, t, "{{c$rhs_desolve}}", private$ptr,
                            initfunc = "{{c$initmod_desolve}}",
                            nout = private$n_out, dllname = "{{package}}",
                            tcrit = tcrit, ...)
      }

      ## NOTE: This won't work in the case where many dde options are
      ## used; we should at least warn about this in the docs. Support
      ## in dde is hard because we assume that output and y and time
      ## can easily be done together, assuming the time names etc.
      if (use_names) {
        colnames(ret) <- private$ynames
      }

      ret
    }
  ))


## TODO: work out how to get the class added here, and the reference
## to the class so that we can call the ir method as a static method,
## or add it on here.
{{name}} <- function(..., user = list(), use_dde = FALSE) {
  {{name}}_$new(..., user = user, use_dde = use_dde)
}
class({{name}}) <- "odin_generator"
attr({{name}}, "generator") <- {{name}}_
