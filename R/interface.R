##' Return detailed information about an odin model.  This is the
##' mechanism through which \code{\link{coef}} works with odin.
##'
##' @section Warning:
##'
##' The returned data is subject to change for a few versions while I
##'   work out how we'll use it.
##'
##' @title Return detailed information about an odin model
##'
##' @param x An \code{odin_generator} function, as created by
##'   \code{\link{odin}}
##'
##' @param parsed Logical, indicating if the representation should be
##'   parsed and converted into an R object.  If \code{FALSE} we
##'   return a json string.
##'
##' @export
##' @examples
##' exp_decay <- odin::odin({
##'   deriv(y) <- -0.5 * y
##'   initial(y) <- 1
##' }, target = "r")
##' odin::odin_ir(exp_decay)
##' coef(exp_decay)
odin_ir <- function(x, parsed = FALSE) {
  if (inherits(x, "odin_generator")) {
    ir <- attr(x, "ir")
  } else if (inherits(x, "odin_model")) {
    ir <- x$ir
  } else {
    stop("Expected an odin_generator or odin_model object")
  }

  if (!inherits(ir, "json")) {
    if (length(ir) == 2L) {
      ir <- system.file(ir[[2]], package = ir[[1]], mustWork = TRUE)
    }
    ir <- read_string(ir)
    class(ir) <- "json"
  }
  if (parsed) {
    ir <- ir_deserialise(ir)
  }
  ir
}


##' @export
##' @importFrom stats coef
coef.odin_generator <- function(object, ...) {
  dat <- odin_ir(object, TRUE)

  name <- names(dat$user)
  user <- unname(dat$equations[name])
  default_value <- unname(lapply(user, function(x) x$user$default))
  has_default <- !vlapply(default_value, is.null)
  min <- vnapply(user, function(x) x$user$min %||% -Inf)
  max <- vnapply(user, function(x) x$user$max %||%  Inf)
  integer <- vlapply(user, function(x) x$user$integer %||% FALSE)
  rank <- viapply(dat$data$elements[name], "[[", "rank", USE.NAMES = FALSE)

  data.frame(name = name,
             has_default = has_default,
             default_value = I(default_value),
             rank = rank,
             min = min,
             max = max,
             integer = integer,
             stringsAsFactors = FALSE)
}


##' @export
coef.odin_model <- coef.odin_generator


##' @export
print.odin_generator <- function(x, ...) {
  cat(paste0(format(x, ...), "\n", collapse = ""))
  invisible(x)
}


##' @export
format.odin_generator <- function(x, ...) {
  args <- utils::capture.output(args(x))
  args <- args[-length(args)] # drop 'NULL'
  user_info <- coef(x)
  c(args,
    "<an 'odin_generator' function>",
    if (nrow(user_info) > 0L)
      "  use coef() to get information on user parameters")
}
