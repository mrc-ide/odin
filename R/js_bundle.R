##' Create a JavaScript bundle of an odin model
##'
##' @section Warning:
##'
##' The interface and generated code here are subject to change. As it
##'   stands, it does what is needed for our work in
##'   [odin.api](https://github.com/mrc-ide/odin.api) and does not
##'   actually produce a useful bundle!
##'
##' @title Create a bundle of an odin model
##'
##' @param code An expression, string or path to a file containing
##'   odin code (as for [odin::odin_parse_]). If `NULL`, compile no
##'   model and return only the support code.
##'
##' @param include_support Logical, indicating if the support code
##'   should be included. Without this you need to manually copy over
##'   odin.js or dust.js depending on what model type you have.
##'
##' @return A list, with contents subject to change.
##'
##' @export
##' @examples
##' js <- odin::odin_js_bundle(quote({
##'   deriv(x) <- 1
##'   initial(x) <- 1
##' }), include_support = FALSE)
##' head(js$model$code, 20)
odin_js_bundle <- function(code, include_support = TRUE) {
  ret <- list()

  options <- odin_options(target = "js")
  ## It's not clear why this step is so slow, occasionally -
  ## possibly the bytecode compiler?
  ir <- odin_parse_(code, options)
  dat <- generate_js(ir, options)
  ret$model <- list(code = dat$code, name = dat$name)
  ret$is_discrete <- dat$features$discrete
  ret$support_file <- if (ret$is_discrete) "dust.js" else "odin.js"

  if (include_support) {
    ## TODO: Better than warn = FALSE, add a newline when saving code
    ## into the package.
    ret$support <- readLines(odin_file(file.path("js", ret$support_file)),
                             warn = FALSE)
  }

  ret
}
