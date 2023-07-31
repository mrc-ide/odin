odin_js_wrapper <- function(ir, options) {
  res <- generate_js(ir, options)
  odin_js_wrapper_object(res)
}


##' @importFrom R6 R6Class
odin_js_wrapper_object <- function(res) {
  if (res$features$discrete) {
    ret <- odin_js_wrapper_discrete(res)
  } else {
    ret <- odin_js_wrapper_continuous(res)
  }
  ## TODO: we should drop this I think, and move to the same
  ## workaround as the main odin generators soon.
  class(ret) <- c("odin_js_generator", class(ret))
  ret
}


odin_js_wrapper_continuous <- function(res) {
  context <- new_context("odin.js", res$code)
  has_delay <- res$features$has_delay
  private <- NULL
  R6::R6Class(
    "odin_model",
    cloneable = FALSE,

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
        js_eval(private$context, sprintf("delete %s;", private$name))
      },

      update_metadata = function() {
        metadata <- js_call(private$context,
                            sprintf("%s.getMetadata", private$name))
        private$internal_order <- metadata$internalOrder
        private$variable_order <- metadata$variableOrder
        private$output_order <- metadata$outputOrder
      }
    ),

    public = list(
      initialize = function(..., user = list(...), unused_user_action = NULL) {
        private$name <- sprintf("%s.%s", JS_INSTANCES, basename(tempfile("i")))

        user_js <- to_js_user(user)
        unused_user_action <- unused_user_action %||%
          getOption("odin.unused_user_action", "warning")

        init <- sprintf("%s = new odinjs.PkgWrapper(%s, %s, %s);",
                        private$name, private$generator,
                        user_js, dquote(unused_user_action))
        js_eval(private$context, init)
        private$update_metadata()
      },

      initial = function(t) {
        t_js <- to_json_js(scalar(t))
        js_call(private$context, sprintf("%s.initial", private$name), t_js)
      },

      ir = function() {
        private$ir_
      },

      set_user = function(..., user = list(...), unused_user_action = NULL) {
        unused_user_action <- unused_user_action %||%
          getOption("odin.unused_user_action", "warning")
        user_js <- to_js_user(user)
        js_call(private$context, sprintf("%s.setUser", private$name),
                        user_js, unused_user_action)
        private$update_metadata()
      },

      deriv = function(t, y) {
        t_js <- to_json_js(scalar(t))
        y_js <- to_json_js(y, auto_unbox = FALSE)
        res <- js_call(private$context, sprintf("%s.rhs", private$name),
                       t_js, y_js)
        if (length(res$output) == 0) {
          res$state
        } else {
          structure(res$state, output = res$output)
        }
      },

      contents = function() {
        ret <- js_call(private$context, sprintf("%s.getInternal", private$name))
        order <- private$internal_order
        for (i in names(ret)) {
          d <- order[[i]]
          if (length(d) > 1) {
            dim(ret[[i]]) <- d
          }
        }
        if (has_delay && is.null(ret$initial_t)) {
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
        y_js <- to_json_js(y, auto_unbox = FALSE, null = "null")
        control <- list(atol = atol,
                        rtol = rtol,
                        tcrit = tcrit,
                        maxSteps = step_max_n,
                        stepSizeMin = step_size_min,
                        stepSizeMax = step_size_max,
                        stepSizeMinAllow = step_size_min_allow)
        control <- control[!vlapply(control, is.null)]
        control_js <- to_json_js(control, auto_unbox = TRUE)

        res <- js_call(private$context, sprintf("%s.run", private$name),
                       t_js, y_js, control_js)

        ## Need to add time on
        y <- cbind(t, res$y, deparse.level = 0)
        if (use_names) {
          colnames(y) <- c(TIME, res$names)
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
}


## This is very similar to the above, but just different enough to
## make it really annoying to try and do with if/else type logic.
odin_js_wrapper_discrete <- function(res) {
  context <- new_context("dust.js", res$code)
  private <- NULL
  R6::R6Class(
    "odin_model",
    cloneable = FALSE,

    private = list(
      context = context,
      generator = res$name,
      name = NULL,
      features = res$features,
      metadata = NULL,
      ir_ = res$ir,

      finalize = function() {
        js_eval(private$context, sprintf("delete %s;", private$name))
      },

      update_metadata = function() {
        private$metadata <-
          js_call(private$context, sprintf("%s.getMetadata", private$name))
        info <- private$metadata$info
        ## We need to do this bit of processing in R not JS to
        ## guarantee ordering (i.e., we can't just save the object in
        ## js). There's also a small tweak to convert the empty vector
        ## into NULL values, as that's what transform_variables assumes.
        order <- set_names(as.list(info$dim), info$name)
        order[vlapply(order, function(el) length(el) == 0)] <- list(NULL)
        private$metadata$order <- order
      }
    ),

    public = list(
      initialize = function(..., user = list(...), unused_user_action = NULL) {
        private$name <- sprintf("%s.%s", JS_INSTANCES, basename(tempfile("i")))
        user_js <- to_js_user(user)
        unused_user_action <- unused_user_action %||%
          getOption("odin.unused_user_action", "warning")
        init <- sprintf("%s = new dust.PkgWrapper(%s, %s, %s, %s);",
                        private$name, private$generator, user_js,
                        dquote(unused_user_action), odin_js_dust_rng())
        js_eval(private$context, init)
        private$update_metadata()
      },

      initial = function(step) {
        step_js <- to_json_js(scalar(step))
        js_call(private$context, sprintf("%s.initial", private$name), step_js)
      },

      ir = function() {
        private$ir_
      },

      set_user = function(..., user = list(...), unused_user_action = NULL) {
        unused_user_action <- unused_user_action %||%
          getOption("odin.unused_user_action", "warning")
        user_js <- to_js_user(user)
        js_call(private$context, sprintf("%s.setUser", private$name),
                user_js, unused_user_action)
        private$update_metadata()
      },

      update = function(step, y) {
        step_js <- to_json_js(scalar(step))
        y_js <- to_json_js(y, auto_unbox = FALSE)
        js_call(private$context, sprintf("%s.update", private$name),
                step_js, y_js)
      },

      contents = function() {
        ret <- js_call(private$context, sprintf("%s.getInternal", private$name))
        for (nm in names(res$internal_dim)) {
          dim(ret[[nm]]) <- vnapply(res$internal_dim[[nm]],
                                    function(x) ret[[x]])
        }
        ret
      },

      run = function(step, y = NULL, ..., use_names = TRUE) {
        step_js <- to_json_js(step, auto_unbox = FALSE)
        y_js <- to_json_js(y, auto_unbox = FALSE, null = "null")
        res <- js_call(private$context, sprintf("%s.run", private$name),
                       step_js, y_js)
        ret <- cbind(
          step,
          matrix(res$y, ncol = private$metadata$size, byrow = TRUE),
          deparse.level = 0)
        if (use_names) {
          colnames(ret) <- c(STEP, private$metadata$names)
        }
        ret
      },

      code = function() {
        res$code
      },

      engine = function() {
        "js"
      },

      transform_variables = function(y) {
        support_transform_variables(
          y,
          list(variable_order = private$metadata$order))
      }
    ))
}


##' @export
coef.odin_js_generator <- function(object, ...) {
  ## This is a workaround until odin drops the constructor interface
  coef(structure(list(), ir = object$private_fields$ir_,
                 class = "odin_generator"))
}


to_js_user <- function(user) {
  to_js <- function(value) {
    if (inherits(value, "JS_EVAL")) {
      ## See mrc-3726 for details
      stop("Direct passing of JS objects not currently supported")
    } else if (is.array(value)) {
      value <- list(data = c(value), dim = I(dim(value)))
    } else if (length(value) == 0) {
    } else if (is.null(value)) { # leave as is
    } else if (length(value) != 1L || inherits(value, "AsIs")) {
      value <- list(data = value, dim = I(length(value)))
    }
    value
  }
  user <- user[!vlapply(user, is.null)]
  if (length(user) == 0) {
    return(V8::JS("{}"))
  }
  stopifnot(!is.null(names(user)))
  user <- lapply(user, to_js)
  args <- jsonlite::toJSON(user, auto_unbox = TRUE, digits = NA,
                           null = "null", na = "null")
  V8::JS(args)
}


new_context <- function(support, code) {
  context <- V8::v8()
  context$source(odin_file(file.path("js", support)))
  context$eval(sprintf("var %s = {};", JS_INSTANCES))
  context$eval(paste(code, collapse = "\n"))
  context
}


js_call <- function(context, ...) {
  tryCatch(context$call(...), error = function(e) stop(e$message))
}


js_eval <- function(context, ...) {
  tryCatch(context$eval(...), error = function(e) stop(e$message))
}


odin_js_dust_rng <- function() {
  code <- readLines(odin_file("js/dust-rng.js"))
  V8::JS(code[!grepl("^//", code)])
}


to_json_js <- function(x, auto_unbox = FALSE, digits = NA, ...) {
  V8::JS(jsonlite::toJSON(x, auto_unbox = auto_unbox, digits = digits, ...))
}


##' Report versions of JavaScript packages used to run odin models.
##'
##' @title Report JS versions
##'
##' @return A named list of [package_version] versions, for `odinjs`
##'   and other components used in the JavaScript support.
##'
##' @export
##' @examples
##' odin::odin_js_versions()
odin_js_versions <- function() {
  context <- V8::v8()
  context$source(odin_file("js/odin.js"))
  context$source(odin_file("js/dust.js"))
  utils::modifyList(
    lapply(context$call("dust.versions"), package_version),
    lapply(context$call("odinjs.versions"), package_version))
}
