ir_serialise <- function(dat, pretty = TRUE) {
  jsonlite::toJSON(dat, null = "null", pretty = pretty, digits = NA)
}


## TODO: we should be able to see in the schema all the cases that are
## character vectors.
ir_deserialise <- function(ir) {
  dat <- jsonlite::fromJSON(ir, simplifyVector = FALSE)
  dat$components <- lapply(dat$components, lapply, list_to_character)

  if (dat$features$has_array) {
    dat$data$data <- lapply(dat$data$data, ir_deserialise_data_dimnames)
  }

  names(dat$data$data) <- vcapply(dat$data$data, "[[", "name")
  names(dat$data$variable$contents) <-
    vcapply(dat$data$variable$contents, "[[", "name")
  names(dat$data$output$contents) <-
    vcapply(dat$data$output$contents, "[[", "name")
  names(dat$equations) <- vcapply(dat$equations, "[[", "name")

  dat$interpolate <- lapply(dat$interpolate, list_to_character)
  dat$equations <- lapply(dat$equations, ir_deserialise_equation)

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
    if (!is.null(eq$delay$subs)) {
      eq$delay$subs <- list_to_character(eq$delay$subs)
    }
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
