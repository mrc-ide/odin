##' Parse an odin model, returning an intermediate representation.
##' The \code{odin_parse_} version is a "standard evaluation" escape
##' hatch.
##'
##' A schema for the intermediate representation is available in the
##' package as \code{schema.json}.  It is subject to change at this
##' point.
##'
##' @title Parse an odin model
##'
##' @param x An expression, character vector or filename with the odin
##'   code
##'
##' @param options odin options; see \code{\link{odin_options}}.  The
##'   primary options that affect the parse stage are \code{validate}
##'   and \code{pretty}.
##'
##' @param type An optional string indicating the the type of input -
##'   must be one of \code{expression}, \code{file} or \code{text} if
##'   provided.  This skips the type detection code used by odin and
##'   makes validating user input easier.
##'
##' @export
##' @author Rich FitzJohn
odin_parse <- function(x, type = NULL, options = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin_parse_(xx, options)
}


##' @export
##' @rdname odin_parse
odin_parse_ <- function(x, options = NULL, type = NULL) {
  options <- odin_options(options = options)
  ir_parse(x, options, type)
}
