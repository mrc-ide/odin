odin_preprocess <- function(x) {
  preprocess_expression <- function(x) {
    if (inherits(x, "{")) {
      as.expression(as.list(x[-1L]))
    } else {
      as.expression(x)
    }
  }

  type <- odin_preprocess_detect(x)

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

odin_preprocess_detect <- function(x) {
  if (is.language(x)) {
    as <- "expression"
  } else if (is.character(x)) {
    if (length(x) > 1L || grepl("([\n;=()]|<-)", x)) {
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
