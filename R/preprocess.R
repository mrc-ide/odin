odin_preprocess <- function(x, type = NULL) {
  preprocess_expression <- function(x) {
    if (inherits(x, "{")) {
      as.expression(as.list(x[-1L]))
    } else {
      as.expression(x)
    }
  }

  type <- odin_preprocess_detect(x, type)

  exprs <- switch(type,
                  file = parse(file = x, keep.source = TRUE),
                  text = parse(text = x, keep.source = TRUE),
                  expression = preprocess_expression(x))

  if (type == "file") {
    file <- x
    path <- c(normalizePath(dirname(x)), normalizePath(getwd()))
  } else {
    file <- NULL
    path <- getwd()
  }

  ret <- list(type = type,
              path = path,
              file = file,
              exprs = exprs)

  ## TODO: This is a bit of a hack to avoid rewriting all the uses of
  ## odin_parse in the tests.  They could be rewritten using a fn
  ##   odin_parse2 <- function(x) odin_parse(odin_preprocess(x))
  ## perhaps?
  attr(ret, "odin_preprocessed") <- TRUE
  ret
}

odin_preprocess_detect <- function(x, type = NULL) {
  has_type <- !is.null(type)
  if (has_type) {
    type <- match_value(type, c("file", "text", "expression"))
  }

  if (is.language(x)) {
    as <- "expression"
  } else if (is.character(x)) {
    if (has_type) {
      if (type == "expression") {
        stop("Invalid input for odin - expected expression", call. = FALSE)
      } else if (type == "file") {
        stopifnot(length(x) == 1, is.character(x), !is.na(x))
        if (!file.exists(x)) {
          stop(sprintf("File '%s' does not exist", x), call. = FALSE)
        }
      }
      as <- type
    } else if (length(x) > 1L || grepl("([\n;=()]|<-)", x)) {
      as <- "text"
    } else if (file.exists(x)) {
      as <- "file"
    } else {
      stop("'x' looks like a filename, but file does not exist")
    }
  } else {
    stop("Invalid type for 'x'")
  }
  as
}
