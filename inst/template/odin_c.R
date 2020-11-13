{{name}}_ <- R6::R6Class(
  "odin_model",
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
    cfuns = {{cfuns}},
    dll = "{{package}}",
    user = {{user}},

    ## This is never called, but is used to ensure that R finds our
    ## symbols that we will use from the package; without this they
    ## cannot be found by dynamic lookup now that we use the package
    ## FFI registration system.
    registration = function() {
      if (FALSE) {
        {{registration}}
      }
    },

    ## This only does something in delay models
    set_initial = function({{time}}, y, use_dde) {
      .Call("{{c$set_initial}}", private$ptr, {{time}}, y, use_dde,
            PACKAGE= "{{package}}")
    },

    update_metadata = function() {
      meta <- .Call("{{c$metadata}}", private$ptr,
                    PACKAGE = "{{package}}")
      private$variable_order <- meta$variable_order
      private$output_order <- meta$output_order
      private$n_out <- meta$n_out
      private$ynames <- private$odin$make_names(
        private$variable_order, private$output_order, {{discrete}})
      private$interpolate_t <- meta$interpolate_t
    }
  ),

  public = list(
    ## TODO: Deprecate (fairly agressively) use_dde; directly from the
    ## constructor should be NULL and via the generator should be
    ## FALSE with warning? What else uses it? We might need to change
    ## the behaviour of set_initial.
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

    ## Do we need to have the user-settable args here? It would be
    ## nice, but that's not super straightforward to do.
    set_user = function(..., user = list(...), unused_user_action = NULL) {
      support_check_user(user, private$user, unused_user_action)
      .Call("{{c$set_user}}", private$ptr, user, PACKAGE = "{{package}}")
      private$update_metadata()
    },

    ## This might be time sensitive and, so we can avoid computing
    ## it. I wonder if that's an optimisation we should drop for now
    ## as it does not seem generally useful. This would bring us
    ## closer to the js version which requires that we always pass the
    ## time in.
    initial = function({{time}}) {
      .Call("{{c$initial}}", private$ptr, {{time}}, PACKAGE = "{{package}}")
    },

    rhs = function({{time}}, y) {
      .Call("{{c$rhs_r}}", private$ptr, {{time}}, y, PACKAGE = "{{package}}")
    },

    {{rhs}} = function({{time}}, y) {
      ## .Deprecated("$rhs()")
      self$rhs({{time}}, y)
    },

    contents = function() {
      .Call("{{c$contents}}", private$ptr, PACKAGE = "{{package}}")
    },

    transform_variables = function(y) {
      private$odin$support_transform_variables(y, private)
    },

    run = function({{time}}, y = NULL, ..., use_names = TRUE) {
      private$odin${{run}}(
        self, private, {{time}}, y, ..., use_names = use_names)
    }
  ))


{{name}} <- function(..., user = list(...), use_dde = FALSE,
                     unused_user_action = NULL) {
  {{name}}_$new(user = user, use_dde = use_dde,
                unused_user_action = unused_user_action)
}
class({{name}}) <- "odin_generator"
attr({{name}}, "generator") <- {{name}}_
