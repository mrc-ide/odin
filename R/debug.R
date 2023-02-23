## Handle parsing of a print string, via glue
debug_parse_string <- function(string) {
  seen <- collector()
  transformer <-  function(text, envir) {
    seen$add(trimws(text))
    text
  }
  glue::glue(string, .transformer = transformer)
  seen$get()
}


debug_substitute_string <- function(string, values) {
  seen <- collector()
  transformer <-  function(text, envir) {
    seen$add(text)
    values[[seen$length()]]
  }
  glue::glue(string, .transformer = transformer)
}


debug_parse_element <- function(str) {
  re <- "(.+)\\s*;\\s*(.+)"
  has_format <- grepl(re, str)
  if (has_format) {
    format <- sub(re, "\\2", str)
    ## Try applying the format in; we'll error here and be caught
    ## later if this is not interpretable.
    sprintf(paste0("%", format), 1)    
    value <- sub(re, "\\1", str)
  } else {
    format <- NULL
    value <- str
  }

  expr <- parse(text = value)[[1]]
  depends <- find_symbols(expr)

  return(list(expr = expr, depends = depends, format = format))
}


## This is a bit tedious, we could work with match.call but it's a
## bit too magic.
debug_parse_print_call <- function(args, line, source) {
  if (length(args) == 0) {
    ir_parse_error("print() expects at least one argument", line, source)
  }
  if (!is.null(names(args)) && nzchar(names(args)[[1]])) {
    ir_parse_error("print() expects the first argument to be unnamed",
                   line, source)
  }

  expr <- args[[1]]
  args <- as.list(args[-1])

  if (!is.character(expr)) {
    ir_parse_error("print() a string argument", line, source)
  }

  if (length(args) > 0 && (is.null(names(args)) || any(!nzchar(names(args))))) {
    ir_parse_error("print() expects every argument but the first to be named",
                   line, source)
  }

  args_allowed <- "when"
  err <- setdiff(names(args), args_allowed)
  if (length(err) > 0) {
    ir_parse_error(sprintf("Unknown argument to print(); %s",
                           paste(squote(err), collapse = ", ")),
                   line, source)
  }

  list(type = "print",
       expr = expr,
       when = args$when)
}


check_format <- function(fmt) {
  sprintf(paste0("%", fmt), 1)
  fmt
}
