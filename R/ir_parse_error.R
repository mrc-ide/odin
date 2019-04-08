## We're going to need to wrap this up like testthat I think, so that
## we can catch these and group them together.  But leaving that for
## now.
ir_parse_error <- function(msg, line, source) {
  ret <- ir_parse_error_data(msg, unique(line), source, "error")
  class(ret) <- c("odin_error", "error", "condition")
  stop(ret)
}


ir_parse_note <- function(msg, line, source) {
  announce <- .odin$note_function %||% function(x) message(x$message)
  ret <- ir_parse_error_data(msg, unique(line), source, "message")
  announce(ret)
}


ir_parse_error_data <- function(msg, line, source, type) {
  if (length(line) > 0L) {
    expr <- source[line]
    str <- sprintf(ifelse(is.na(line), "%s", "%s # (line %s)"), expr, line)
    message <- paste0(msg, paste0("\n\t", str, collapse = ""))
  } else {
    ## There are not many cases that will trigger this - the most used
    ## one is where we have no equations at all.  The other (more
    ## used) case is in testing.
    expr <- NULL
    message <- msg
  }
  list(message = message,
       msg = msg,
       line = line,
       expr = expr,
       type = type)
}


ir_parse_error_lines <- function(eqs) {
  unlist(unname(lapply(eqs, "[[", "source")))
}
