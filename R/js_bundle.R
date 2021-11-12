##' Create a JavaScript bundle of an odin model
##'
##' @section Warning:
##'
##' The inteface and generated code here are subject to change.
##'
##' @title Create a bundle of an odin model
##'
##' @param code An expression, string or path to a file containing
##'   odin code (as for [odin::odin_parse_]
##'
##' @param include Optional vector of paths of filenames to include
##'   into the javascript bundle
##'
##' @param include_dopri Logical, indicating if the dopri solver
##'   should be included as well.
##'
##' @export
##' @examples
##' js <- odin::odin_js_bundle({
##'   deriv(x) <- 1
##'   initial(x) <- 1
##' }, include_dopri = FALSE)
##' head(js)
odin_js_bundle <- function(code,
                           include = NULL,
                           include_dopri = TRUE) {
  options <- odin_options(target = "js")
  ir <- odin_parse_(code, options)
  dat <- generate_js(ir, options)

  support <- c(
    if (include_dopri) "dopri.js",
    "support.js",
    names(which(dat$include)))
  support_js <- lapply(odin_file(file.path("js", support)),
                       readLines, warn = FALSE)

  if (!is.null(include)) {
    include <- js_flatten_eqs(lapply(include, readLines))
  }

  ## TODO: Revisit this now that there's only one generator and work
  ## out what a sensible return type would be. This depends to a
  ## degree on how things want to be pushed around in wodin.
  c(js_flatten_eqs(support_js),
    sprintf("var %s = {};", JS_GENERATORS),
    dat$code,
    include)
}


##' Generate a web page and javascript for a built-in example
##'
##' @title Generate a built in example
##'
##' @param filename Filename of model to include
##'
##' @param dest Destination directory - multiple files will be created
##'   here, overwriting existing files without prompting.
##'
##' @export
odin_js_example <- function(filename, dest = tempfile()) {
  ## This is intended to eventually be configurable
  path <- odin_file("js/example/simple")
  include <- dir(path, pattern = "\\.js$", full.names = TRUE)
  html <- dir(path, pattern = "\\.html$", full.names = TRUE)

  dir.create(dest, FALSE, TRUE)
  odin_js_bundle(filename, file.path(dest, "odin.js"), include = include)
  file.copy(html, dest, overwrite = TRUE)
  dest
}
