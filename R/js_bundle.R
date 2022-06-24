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
##'   should be included (the solver and common driver code)
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
odin_js_bundle <- function(code, include_support = TRUE) {
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
    ## Better than warn = FALSE, add a newline when saving code into
    ## the package.
    ret$support <- readLines(odin_file(file.path("js", "wodin-runner.js")),
                             warn = FALSE)
  }

  ret
}
