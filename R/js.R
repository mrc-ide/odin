odin_js_wrapper <- function(ir, options) {
  res <- generate_js(ir, options)
  odin_js_wrapper_object(res)
}


##' @importFrom R6 R6Class
odin_js_wrapper_object <- function(res) {
  context <- js_context(names(which(res$include)))
  context$eval(paste(res$code, collapse = "\n"))

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
        private$internal_order <-
          private$context$get(
            sprintf("%s.metadata.internalOrder", private$name))
        private$variable_order <-
          private$context$get(
            sprintf("%s.metadata.variableOrder", private$name))
        private$output_order <-
          private$context$get(
            sprintf("%s.metadata.outputOrder", private$name))
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
        ## For compatibility with odin, without having to write the full
        ## interface
        if (length(user) > 0L && !private$features$has_user) {
          tryCatch(do.call(function() NULL, user),
                   error = function(e) stop(e$message, call. = FALSE))
        }

        user_js <- to_json_user(user)
        unused_user_action <- unused_user_action %||%
          getOption("odin.unused_user_action", "warning")
        init <- sprintf("%s = new %s.%s(%s, %s);",
                        private$name, JS_GENERATORS, private$generator,
                        user_js, dquote(unused_user_action))
        if (private$features$discrete) {
          self$update <- self$rhs
        } else {
          self$deriv <- self$rhs
        }
        private$js_eval(init)
        private$update_metadata()

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
        user_js <- to_json_user(user)
        private$js_call(sprintf("%s.setUser", private$name),
                        user_js, unused_user_action)
        private$update_metadata()
      },

      rhs = function(t, y) {
        ## TODO: check length of 'y' here?
        t_js <- to_json_js(scalar(t))
        y_js <- to_json_js(y, auto_unbox = FALSE)
        ret <- private$js_call(sprintf("%s.rhsEval", private$name),
                               t_js, y_js)
        ## This is super ugly but should do the trick for now:
        if (length(ret) > length(y)) {
          i <- seq_along(y)
          ret <- structure(ret[i], output = ret[-i])
        }
        ret
      },

      contents = function() {
        ret <- private$context$get(sprintf("%s.internal", private$name))
        order <- private$internal_order
        for (i in names(ret)) {
          d <- order[[i]]
          if (length(d) > 1) {
            dim(ret[[i]]) <- d
          }
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

        ## NOTE: tcrit here is ignored when calling the discrete time
        ## model
        res <- private$js_call(sprintf("%s.run", private$name),
                               t_js, y_js, control_js)
        if (use_names) {
          colnames(res$y) <- res$names
        }

        if (return_statistics) {
          ## Convert into the same as for dde, which is a subset (we
          ## discard here lastError, stiffNNonstiff and stiffNStiff)
          statistics <- c(n_eval = res$statistics$nEval,
                          n_step = res$statistics$nSteps,
                          n_accept = res$statistics$nStepsAccepted,
                          n_reject = res$statistics$nStepsRejected)
          attr(res$y, "statistics") <- statistics
        }
        res$y
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


##' @importFrom V8 v8
js_context <- function(include) {
  ct <- V8::v8()

  ct$source(odin_file("js/dopri.js"))
  ct$source(odin_file("js/support.js"))
  for (f in include) {
    ct$source(odin_file(file.path("js", f)))
  }

  ## TODO: once working drop this GENERATORS bit
  ct$eval(sprintf("var %s = {};", JS_GENERATORS))
  ct$eval(sprintf("var %s = {};", JS_INSTANCES))
  ct
}


##' @export
coef.odin_js_generator <- function(object, ...) {
  ## This is a workaround until odin drops the constructor interface
  coef(structure(list(), ir = object$private_fields$ir_,
                 class = "odin_generator"))
}


to_json_user <- function(user) {
  f <- function(x) {
    if (inherits(x, "JS_EVAL")) {
      class(x) <- "json"
    } else if (is.array(x)) {
      x <- list(data = c(x), dim = I(dim(x)))
    } else if (is.null(x)) { # leave as is
    } else if (length(x) != 1L || inherits(x, "AsIs")) {
      x <- list(data = x, dim = I(length(x)))
    }
    x
  }
  if (length(user) > 0) {
    stopifnot(!is.null(names(user)))
  }
  user <- lapply(user, f)
  to_json_js(user, auto_unbox = TRUE, json_verbatim = TRUE, null = "null")
}


to_json_js <- function(x, auto_unbox = FALSE, digits = NA, ...) {
  V8::JS(jsonlite::toJSON(x, auto_unbox = auto_unbox, digits = digits, ...))
}
