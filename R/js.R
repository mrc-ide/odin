odin_js_wrapper <- function(ir, options) {
  res <- generate_js(ir, options)
  odin_js_wrapper_object(res)
}


##' @importFrom R6 R6Class
odin_js_wrapper_object <- function(res) {
  ## New js_context:
  context <- V8::v8()
  context$source(odin_file("js/odin.js"))
  context$eval(sprintf("var %s = {};", JS_INSTANCES))

  ## Then our new code:
  context$eval(paste(res$code, collapse = "\n"))

  is_discrete <- res$features$discrete
  is_continuous_delay <- res$features$has_delay && !is_discrete
  private <- NULL

  ret <- R6::R6Class(
    "odin_model",
    cloneable = FALSE,
    lock_objects = FALSE,

    private = list(
      context = context,
      generator = res$name,
      name = NULL,
      features = res$features,
      internal_order = NULL,
      variable_order = NULL,
      output_order = NULL,
      ir_ = res$ir,

      finalize = function() {
        private$js_eval(sprintf("delete %s;", private$name))
      },

      update_metadata = function() {
        metadata <- private$js_call(sprintf("%s.getMetadata", private$name))
        private$internal_order <- metadata$internalOrder
        private$variable_order <- metadata$variableOrder
        private$output_order <- metadata$outputOrder
      },

      js_call = function(...) {
        tryCatch(private$context$call(...), error = function(e) stop(e$message))
      },

      js_eval = function(...) {
        tryCatch(private$context$eval(...), error = function(e) stop(e$message))
      }
    ),

    public = list(
      initialize = function(..., user = list(...), unused_user_action = NULL) {
        private$name <- sprintf("%s.%s", JS_INSTANCES, basename(tempfile("i")))

        user_js <- to_js_user(user)
        unused_user_action <- unused_user_action %||%
          getOption("odin.unused_user_action", "warning")

        init <- sprintf("%s = new wodinRunner.PkgWrapper(%s, %s, %s);",
                        private$name, private$generator,
                        user_js, dquote(unused_user_action))
        private$js_eval(init)
        private$update_metadata()

        if (private$features$discrete) {
          self$update <- self$rhs
        } else {
          self$deriv <- self$rhs
        }

        lockEnvironment(self)
      },

      initial = function(t) {
        t_js <- to_json_js(scalar(t))
        private$js_call(sprintf("%s.initial", private$name), t_js)
      },

      ir = function() {
        private$ir_
      },

      set_user = function(..., user = list(...), unused_user_action = NULL) {
        unused_user_action <- unused_user_action %||%
          getOption("odin.unused_user_action", "warning")
        user_js <- to_js_user(user)
        private$js_call(sprintf("%s.setUser", private$name),
                        user_js, unused_user_action)
        private$update_metadata()
      },

      rhs = function(t, y) {
        t_js <- to_json_js(scalar(t))
        y_js <- to_json_js(y, auto_unbox = FALSE)
        res <- private$js_call(sprintf("%s.rhs", private$name), t_js, y_js)
        if (length(res$output) == 0) {
          res$state
        } else {
          structure(res$state, output = res$output)
        }
      },

      contents = function() {
        ret <- private$js_call(sprintf("%s.getInternal", private$name))
        order <- private$internal_order
        for (i in names(ret)) {
          d <- order[[i]]
          if (length(d) > 1) {
            dim(ret[[i]]) <- d
          }
        }
        if (is_continuous_delay && is.null(ret$initial_t)) {
          ## NaN serialises to NULL, which is not quite what we want
          ret$initial_t <- NA_real_
        }
        ret
      },

      run = function(t, y = NULL, ..., tcrit = NULL, atol = NULL, rtol = NULL,
                     step_max_n = NULL, step_size_min = NULL,
                     step_size_max = NULL, step_size_min_allow = NULL,
                     use_names = TRUE, return_statistics = FALSE) {
        t_js <- to_json_js(t, auto_unbox = FALSE)
        if (is.null(y)) {
          y_js <- V8::JS("null")
        } else {
          y_js <- to_json_js(y, auto_unbox = FALSE)
        }
        control <- list(atol = atol,
                        rtol = rtol,
                        tcrit = tcrit,
                        maxSteps = step_max_n,
                        stepSizeMin = step_size_min,
                        stepSizeMax = step_size_max,
                        stepSizeMinAllow = step_size_min_allow)
        control <- control[!vlapply(control, is.null)]
        control_js <- to_json_js(control, auto_unbox = TRUE)

        res <- private$js_call(sprintf("%s.run", private$name), t_js, y_js,
                               control_js)

        ## Need to add time on
        y <- cbind(t, res$y, deparse.level = 0)
        if (use_names) {
          colnames(y) <- c(if (is_discrete) STEP else TIME, res$names)
        }

        if (return_statistics) {
          ## Convert into the same as for dde, which is a subset (we
          ## discard here lastError, stiffNNonstiff and stiffNStiff)
          statistics <- c(n_eval = res$statistics$nEval,
                          n_step = res$statistics$nSteps,
                          n_accept = res$statistics$nStepsAccepted,
                          n_reject = res$statistics$nStepsRejected)
          attr(y, "statistics") <- statistics
        }
        y
      },

      code = function() {
        res$code
      },

      engine = function() {
        "js"
      },

      transform_variables = function(y) {
        support_transform_variables(y, private)
      }
    ))

  ## TODO: we should drop this I think, and move to the same
  ## workaround as the main odin generators soon.
  class(ret) <- c("odin_js_generator", class(ret))
  ret
}


##' @export
coef.odin_js_generator <- function(object, ...) {
  ## This is a workaround until odin drops the constructor interface
  coef(structure(list(), ir = object$private_fields$ir_,
                 class = "odin_generator"))
}


to_js_user <- function(user) {
  to_js <- function(name, value) {
    if (inherits(value, "JS_EVAL")) {
      class(value) <- "json"
    } else if (is.array(value)) {
      value <- list(data = c(value), dim = I(dim(value)))
    } else if (length(value) == 0) {
    } else if (is.null(value)) { # leave as is
    } else if (length(value) != 1L || inherits(value, "AsIs")) {
      value <- list(data = value, dim = I(length(value)))
    }
    list(name, value)
  }
  user <- user[!vlapply(user, is.null)]
  if (length(user) == 0) {
    return(V8::JS("new Map()"))
  }
  if (length(user) > 0) {
    stopifnot(!is.null(names(user)))
  }
  user <- Map(to_js, names(user), user, USE.NAMES = FALSE)
  args <- jsonlite::toJSON(user, auto_unbox = TRUE, digits = NA,
                           null = "null", na = "null")
  V8::JS(sprintf("new Map(%s)", args))
}


to_json_js <- function(x, auto_unbox = FALSE, digits = NA, ...) {
  V8::JS(jsonlite::toJSON(x, auto_unbox = auto_unbox, digits = digits, ...))
}
