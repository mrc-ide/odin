call_odin_bundle_continuous <- function(bundle, user, t0, t1, tn,
                                        control = NULL) {
  stopifnot(!bundle$is_discrete)

  ct <- V8::v8()
  ct$eval(bundle$support)
  ct$eval(bundle$model$code)
  ct$source(odin_file("js/test-continuous.js"))
  odin_js <- V8::JS(bundle$model$name)
  user_js <- to_js_user(user)

  if (length(control) == 0) {
    control_js <- V8::JS("{}")
  } else {
    control_js <- V8::JS(jsonlite::toJSON(control, auto_unbox = TRUE))
  }

  ct$call("call_odin_bundle", odin_js, user_js, t0, t1, tn, control_js)
}


call_odin_bundle_discrete <- function(bundle, user, t0, t1, dt, n_particles) {
  stopifnot(bundle$is_discrete)

  ct <- V8::v8()
  ct$eval(bundle$support)
  ct$eval(bundle$model$code)
  ct$source(odin_file("js/test-discrete.js"))
  odin_js <- V8::JS(bundle$model$name)
  user_js <- to_js_user(user)

  ct$call("call_odin_bundle", odin_js, user_js, t0, t1, dt, n_particles)
}


to_json_max <- function(x) {
  V8::JS(jsonlite::toJSON(x, digits = NA))
}


model_context <- function(x) {
  environment(x$initialize)$private$context
}


model_set_seed <- function(mod, seed) {
  if (mod$engine() == "js") {
    model_context(mod)$call("setSeed", seed)
  } else {
    set.seed(seed)
  }
}


model_random_numbers <- function(mod, name, n, ...) {
  stopifnot(mod$engine() == "js")
  ctx <- V8::v8()
  ctx$source(odin_file("js/dust.js"))
  ctx$source("random.js")
  jsonlite::fromJSON(ctx$call("random", name, n, list(...)))
}


to_json_columnwise <- function(x) {
  V8::JS(jsonlite::toJSON(x, matrix = "columnmajor"))
}


skip_if_no_js <- function() {
  skip_if_not_installed("V8")
  ## Historically we've had issues with the non-standard V8 build on
  ## Fedora, it's not documented what is different there, but it
  ## behaves poorly.
  skip_on_cran()
}

list_to_matrix <- function(x) {
  matrix(unlist(x), ncol = length(x))
}
