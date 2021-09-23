call_odin_bundle <- function(context, name, user, t, y = NULL, control = NULL) {
  context$eval(package_js("test.js"))
  user <- to_json_user(user)
  if (is.null(y)) {
    y <- js_null()
  }
  if (is.null(control)) {
    control_js <- to_json(setNames(list(), character(0)))
  } else {
    control_js <- to_json(control, auto_unbox = TRUE)
  }
  res <- context$call("run", "odin", user, t, y, control_js)
  colnames(res$y) <- res$names
  res$y
}


odin_js_support <- function() {
  v8 <- V8::v8()
  v8$eval(package_js("support.js"))
  v8$eval(package_js("interpolate.js"))
  v8
}


odin_js_test_random <- function(name) {
  skip_if_no_random_js()
  force(name)
  v8 <- V8::v8()
  v8$eval(package_js("random.js"))

  v8$eval(c(
    "var repeat = function(f, n) {",
    "  var ret = [];",
    "  for (var i = 0; i < n; ++i) {",
    "    ret.push(f());",
    "  }",
    "  return ret;",
    "}"))

  ## TODO: set the seed, and make sure we're using seedrandom
  function(n, parameters) {
    f <- V8::JS(sprintf("random.%s(%s)",
                        name, paste(parameters, collapse = ", ")))
    v8$call("repeat", f, n)
  }
}


to_json_max <- function(x) {
  V8::JS(jsonlite::toJSON(x, digits = NA))
}


## Requires newish node:
random_js_supported <- local({
  supported <- NULL
  function() {
    if (is.null(supported)) {
      supported <<- !is.null(tryCatch(
                       V8::v8()$eval(package_js("random.js")),
                       error = function(e) NULL))
    }
    supported
  }
})


skip_if_no_random_js <- function() {
  if (!random_js_supported()) {
    testthat::skip("random.js not supported on your v8 version")
  }
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
