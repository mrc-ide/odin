##' @export
##' @importFrom stats coef
coef.odin_generator <- function(object, ...) {
  attr(object, "user_info")()
}


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
