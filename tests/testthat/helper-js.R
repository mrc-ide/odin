call_odin_bundle <- function(bundle, user, t0, t1, tn, control = NULL) {
  ct <- V8::v8()
  ct$eval(bundle$support)
  ct$eval(bundle$model$code)
  ct$source(odin_file("js/test.js"))
  odin_js <- V8::JS(bundle$model$name)
  user_js <- to_js_user(user)
  if (length(control) == 0) {
    control_js <- to_json(setNames(list(), character(0)))
  } else {
    control_js <- to_json(control, auto_unbox = TRUE)
  }

  res <- ct$call("call_odin_bundle", odin_js, user_js, t0, t1, tn, control_js)

  ## We've had a bit of pain from serialisation here, transpose it:
  lapply(seq_len(nrow(res)), function(i) lapply(res, "[[", i))
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


model_random_numbers <- function(x, name, n, ...) {
  ctx <- model_context(x)
  ctx$eval(c(
    "var repeat = function(f, n) {",
    "  var ret = [];",
    "  for (var i = 0; i < n; ++i) {",
    "    ret.push(f());",
    "  }",
    "  return ret;",
    "}"))
  f <- V8::JS(sprintf("random.%s(%s)", name, paste(c(...), collapse = ", ")))
  ctx$call("repeat", f, n)
}


with_options <- function(opts, code) {
  oo <- options(opts)
  on.exit(oo)
  force(code)
}


to_json_columnwise <- function(x) {
  V8::JS(jsonlite::toJSON(x, matrix = "columnmajor"))
}
