##' Deserialise odin's intermediate model representation from a json
##' string into an R object.  Unlike the json, there is no schema for
##' this representation.  This function provides access to the same
##' deserialisation that odin uses internally so may be useful in
##' applications.
##'
##' @title Deserialise odin's IR
##' @param x An intermediate representation as a json string
##' @return A named list
##' @seealso \code{\link{odin_parse}}
##' @export
##' @examples
##' # Parse a model of exponential decay
##' ir <- odin::odin_parse({
##'   deriv(y) <- -0.5 * y
##'   initial(y) <- 1
##' })
##' # Convert the representation to an R object
##' odin::odin_ir_deserialise(ir)
odin_ir_deserialise <- function(x) {
  if (!inherits(x, "json")) {
    stop("Expected a json string")
  }
  ir_deserialise(x)
}


ir_deserialise <- function(ir) {
  dat <- from_json(ir)
  dat$version <- numeric_version(dat$version)
  dat$components <- lapply(dat$components, lapply, list_to_character)

  if (dat$features$has_array) {
    dat$data$elements <- lapply(dat$data$elements, ir_deserialise_data_dimnames)
  }

  names(dat$data$elements) <- vcapply(dat$data$elements, "[[", "name")
  names(dat$data$variable$contents) <-
    vcapply(dat$data$variable$contents, "[[", "name")
  names(dat$data$output$contents) <-
    vcapply(dat$data$output$contents, "[[", "name")
  names(dat$equations) <- vcapply(dat$equations, "[[", "name")
  names(dat$user) <- vcapply(dat$user, "[[", "name")

  names(dat$config$include) <- vcapply(dat$config$include, "[[", "name")

  dat$interpolate <- lapply(dat$interpolate, list_to_character)
  dat$equations <- lapply(dat$equations, ir_deserialise_equation)
  dat$ir <- ir

  dat
}


ir_deserialise_equation <- function(eq) {
  if (!is.null(eq$depends)) {
    eq$depends <- lapply(eq$depends, list_to_character)
  }
  if (eq$type == "delay_continuous") {
    eq$delay$equations <- list_to_character(eq$delay$equations)
    names(eq$delay$variables$contents) <-
      vcapply(eq$delay$variables$contents, "[[", "name")
    eq$delay$substitutions <-
      set_names(
        vcapply(eq$delay$substitutions, "[[", "to"),
        vcapply(eq$delay$substitutions, "[[", "from"))
  }
  eq
}


ir_deserialise_data_dimnames <- function(x) {
  if (x$rank > 0L) {
    v <- c("dim", "mult")
    x$dimnames[v] <- lapply(x$dimnames[v], list_to_character)
  }
  x
}
