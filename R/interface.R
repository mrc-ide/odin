##' Return detailed information about an odin model.  This is the
##' mechanism through which [coef] works with odin.
##'
##' @section Warning:
##'
##' The returned data is subject to change for a few versions while I
##'   work out how we'll use it.
##'
##' @title Return detailed information about an odin model
##'
##' @param x An `odin_generator` function, as created by
##'   `odin::odin`
##'
##' @param parsed Logical, indicating if the representation should be
##'   parsed and converted into an R object.  If `FALSE` we
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
    ir <- attr(x, "ir") %||% attr(x, "generator")$public_methods$ir()
  } else if (inherits(x, "odin_model")) {
    ir <- x$ir()
  } else {
    stop("Expected an odin_generator or odin_model object")
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
