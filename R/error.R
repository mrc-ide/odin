odin_error <- function(message, call=NULL, type=character()) {
  ret <- list(message=message, call=call)
  class(ret) <- c(type, "odin_error", "error", "condition")
  ret
}

odin_parse_error <- function(expr, message, call=NULL) {
  message <- sprintf("%s\n\t%s # (%s)",
                     message, deparse_str(expr[[1L]]), source_line(expr))
  odin_error(message, call, type="odin_parse_error")
}

pretty_ref <- function(i, expr) {
  sprintf("%s # (%s)", deparse_str(expr[[i]]), source_line(expr[i]))
}

collect_errors <- function(type, expr) {
  pool <- list()
  handle <- function(e) {
    if (inherits(e, type)) {
      pool <<- c(pool, list(e))
      NULL
    } else {
      e
    }
  }
  res <- withCallingHandlers(expr, error=handle)
  if (length(pool) > 0L) {
    stop(paste(vcapply(pool, "[[", "message"), collapse="\n\n"),
         call.=FALSE)
  }
  res
}
