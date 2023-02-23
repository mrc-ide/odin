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
    format <- check_format(sub(re, "\\2", str))
    value <- sub(re, "\\1", str)
  } else {
    format <- NULL
    value <- str
  }
  
  expr <- parse(text = value)[[1]]
  depends <- find_symbols(expr)

  return(list(expr = expr, depends = depends, format = format))  
}


check_format <- function(fmt) {
  sprintf(paste0("%", fmt), 1)
  fmt
}
