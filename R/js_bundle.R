##' Create a JavaScript bundle of an odin model
##'
##' @section Warning:
##'
##' The interface and generated code here are subject to change.
##'
##' @title Create a bundle of an odin model
##'
##' @param code An expression, string or path to a file containing
##'   odin code (as for [odin::odin_parse_]). If `NULL`, compile no
##'   model and return only the support code.
##'
##' @param include_support Logical, indicating if the support code
##'   should be included.
##'
##' @param include_dopri Logical, indicating if the dopri solver
##'   should be included as well.
##'
##' @return A list of character vectors.
##'
##' @export
##' @examples
##' js <- odin::odin_js_bundle(quote({
##'   deriv(x) <- 1
##'   initial(x) <- 1
##' }), include_dopri = FALSE)
##' head(js)
odin_js_bundle <- function(code,
                           include_support = TRUE,
                           include_dopri = TRUE) {
  ret <- list()

  if (!is.null(code)) {
    options <- odin_options(target = "js")
    ## It's not clear why this step is so slow, occasionally -
    ## possibly the bytecode compiler?
    ir <- odin_parse_(code, options)
    dat <- generate_js(ir, options)
    if (dat$features$discrete) {
      stop("Can't cope with discrete models yet")
    }
    ret$model <- list(code = dat$code, name = dat$name)
  }

  if (include_support) {
    ret$support <- readLines(odin_file(file.path("js", "support.js")))
  }
  if (include_dopri) {
    ret$dopri <- readLines(odin_file(file.path("js", "dopri.js")))
  }

  ret
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
  writeLines(
    odin_js_bundle(filename, include = include),
    file.path(dest, "odin.js"))
  file.copy(html, dest, overwrite = TRUE)
  dest
}
