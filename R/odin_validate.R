##' Validate an odin model.  This function is closer to
##' \code{\link{odin_parse_}} than \code{\link{odin_parse}} because it
##' does not do any quoting of the code.  It is primarily intended for
##' use within other applications.
##'
##' \code{odin_validate} will always return a list with the same
##' elements:
##'
##' \describe{
##' \item{success}{A boolean, \code{TRUE} if validation was successful}
##'
##' \item{result}{The intermediate representation, as returned by
##' \code{\link{odin_parse_}}, if the validation was successful,
##' otherwise \code{NULL}}
##'
##' \item{error}{An error object if the validation was unsuccessful,
##' otherwise \code{NULL}.  This may be a classed odin error, in which
##' case it will contain source location information - see the
##' examples for details.}
##'
##' \item{messages}{A list of messages, if the validation returned
##' any.  At present this is only non-fatal information about unused
##' variables.}
##' }
##'
##' @title Validate an odin model
##'
##' @inheritParams odin_parse
##'
##' @export
##' @author Rich FitzJohn
##' @examples
##'
##' # A successful validation:
##' odin::odin_validate(c("deriv(x) <- 1", "initial(x) <- 1"))
##'
##' # A complete failure:
##' odin::odin_validate("")
##'
##' # A more interesting failure
##' code <- c("deriv(x) <- a", "initial(x) <- 1")
##' res <- odin::odin_validate(code)
##' res
##'
##' # The object 'res$error' is an 'odin_error' object:
##' res$error
##'
##' # It contains information that might be used to display to a
##' # user information about the error:
##' unclass(res$error)
##'
##' # Notes are raised in a similar way:
##' code <- c("deriv(x) <- 1", "initial(x) <- 1", "a <- 1")
##' res <- odin::odin_validate(code)
##' res$messages[[1]]
odin_validate <- function(x, type = NULL, options = NULL) {
  msg <- collector_list()
  .odin$note_function <- msg$add
  on.exit(.odin$note_function <- NULL)

  ## NOTE: this does not involve the cache at all, though it possibly
  ## should.  If we do involve the cache we'll need to come up with
  ## something that can be purged or we'll have memory grow without
  ## bounds.
  res <- tryCatch(
    odin_parse_(x, type = type, options = options),
    error = identity)

  success <- !inherits(res, "error")
  error <- if (success) NULL else res
  result <- if (success) res  else NULL

  list(success = success,
       result = result,
       error = error,
       messages = msg$get())
}
