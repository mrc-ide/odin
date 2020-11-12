{{name}}_ <- R6::R6Class(
  "odin",
  cloneable = FALSE,

  private = list(
    ptr = NULL,
    use_dde = NULL,

    odin = NULL,
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
      private$ynames <- private$odin$make_names(
        private$variable_order, private$output_order, FALSE)
      private$interpolate_t <- meta$interpolate_t
    }
  ),

  public = list(
    initialize = function(user = list(), use_dde = FALSE,
                          unused_user_action = NULL) {
      private$odin <- asNamespace("odin")
      private$ptr <- .Call("{{c$create}}", user, PACKAGE = "{{package}}")
      self$set_user(user = user, unused_user_action = unused_user_action)
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
      private$odin$support_transform_variables(y, private)
    },

    run = function(t, y = NULL, ..., use_names = TRUE, tcrit = NULL) {
      t <- as.numeric(t)
      if (is.null(y)) {
        y <- self$initial(t)
      } else {
        y <- as.numeric(t)
      }

      ret <- private$odin$wrapper_run_basic(
        t, y, private$ptr, "{{package}}", private$use_dde,
        "{{c$rhs_dde}}", {{c$output_dde}},
        "{{c$rhs_desolve}}", "{{c$initmod_desolve}}",
        private$n_out, private$interpolate_t, tcrit, ...)

      ## NOTE: This won't work in the case where many dde options are
      ## used; we should at least warn about this in the docs. Support
      ## in dde is hard because we assume that output and y and time
      ## can easily be done together, assuming the time names etc.
      if (use_names) {
        colnames(ret) <- private$ynames
      } else {
        colnames(ret) <- NULL
      }

      ret
    }
  ))


{{name}} <- function(..., user = list(...), use_dde = FALSE) {
  {{name}}_$new(user = user, use_dde = use_dde)
}
class({{name}}) <- "odin_generator"
attr({{name}}, "generator") <- {{name}}_
