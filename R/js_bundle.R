##' Create a JavaScript bundle of odin models
##'
##' @title Create a bundle of odin models
##'
##' @param filenames Filenames with odin source code
##'
##' @param dest Destination file for the generated javascript
##'
##' @param include Optional vector of paths of filenames to include
##'   into the javascript bundle
##'
##' @param include_dopri Logical, indicating if the dopri solver
##'   should be included as well.
##'
##' @export
odin_js_bundle <- function(filenames, dest = tempfile(),
                           include = NULL,
                           include_dopri = TRUE) {
  ## The two options here seem to be: use a vector of paths to source
  ## files or use a path to a directory.  The rest of the interface is
  ## also totally subject to change because we might want to move
  ## those options into the general options interface.  The option to
  ## directly minify from R via V8 would be nice too but probably is
  ## not possible because of the way that the minification process
  ## involves the disk.
  err <- !file.exists(filenames)
  if (any(err)) {
    stop(sprintf("%s not exist: %s",
                 ngettext(sum(err), "File does", "Files do"),
                 paste(squote(filenames[err]), collapse = ", ")))
  }

  options <- odin_options(target = "js")

  f <- function(file) {
    ir <- odin_parse_(file, options)
    generate_js(ir, options)
  }
  dat <- lapply(filenames, f)

  nms <- vcapply(dat, "[[", "name")
  err <- duplicated(nms)
  if (any(err)) {
    stop(sprintf("Duplicate model names: %s",
                 paste(squote(unique(nms[err])), collapse = ", ")))
  }

  needed <- apply(do.call(cbind, lapply(dat, "[[", "include")), 1, any)
  support <- c(
    if (include_dopri) "dopri.js",
    "support.js",
    names(which(needed)))

  if (!is.null(include)) {
    include <- js_flatten_eqs(lapply(include, readLines))
  }
  support_js <- lapply(odin_file(file.path("js", support)),
                       readLines, warn = FALSE)

  code <- c(js_flatten_eqs(support_js),
            sprintf("var %s = {};", JS_GENERATORS),
            js_flatten_eqs(lapply(dat, "[[", "code")),
            include)

  writeLines(code, dest)
  dest
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
  odin_js_bundle(filename, file.path(dest, "odin"), include = include)
  file.copy(html, dest, overwrite = TRUE)
  dest
}
