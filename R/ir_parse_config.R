ir_parse_config <- function(eqs, base_default, root, source, read_include) {
  i <- vcapply(eqs, "[[", "type") == "config"

  config <- lapply(unname(eqs[i]), ir_parse_config1, source)

  nms <- vcapply(config, function(x) x$lhs$name_data)

  base <- ir_parse_config_base(config[nms == "base"], base_default, source)
  include <- ir_parse_config_include(config[nms == "include"], root, source,
                                     read_include)

  list(base = base, include = include)
}


ir_parse_config_base <- function(config, base_default, source) {
  if (length(config) == 0L) {
    base <- base_default
    base_line <- NULL
  } else if (length(config) == 1L) {
    base <- config[[1]]$rhs$value
    base_line <- config[[1]]$source
  } else {
    ir_parse_error("Expected a single config(base) option",
                   ir_parse_error_lines(config), source)
  }
  if (!is_c_identifier(base)) {
    ir_parse_error(
      sprintf("Invalid base value: '%s', must be a valid C identifier", base),
      base_line, source)
  }

  base
}


ir_parse_config_include <- function(include, root, source, read_include) {
  if (length(include) == 0) {
    return(NULL)
  }

  ## First check that the paths exist:
  filename <- vcapply(include, function(x) x$rhs$value)
  filename_full <- file.path(root, filename)
  msg <- !file.exists(filename_full)
  if (any(msg)) {
    ## Only throw here on the first, for simplicity
    x <- include[msg][[1]]
    ir_parse_error(
      sprintf("Could not find file '%s' (relative to root '%s')",
              x$rhs$value, root),
      x$source, source)
  }

  ## TODO: do we want to inject the filename in here?
  res <- lapply(filename_full, function(path)
    withCallingHandlers(
      read_include(path),
      error = function(e) message(sprintf("While reading '%s'", path))))

  nms <- unlist(lapply(res, "[[", "names"))
  dups <- unique(nms[duplicated(nms)])
  if (length(dups) > 0L) {
    lines <- vnapply(include, "[[", "source")
    ir_parse_error(sprintf("Duplicated function %s while reading includes",
                           paste(squote(dups), collapse = ", ")),
                   lines, source)
  }

  list(names = nms,
       data = lapply(res, "[[", "data"))
}


ir_parse_config1 <- function(eq, source) {
  target <- eq$lhs$name_data
  value <- eq$rhs$value

  expected_type <- switch(
    target,
    base = "character",
    include = "character",
    ir_parse_error(sprintf("Unknown configuration option: %s", target),
                   eq$source, source))

  if (!is.atomic(value)) {
    ir_parse_error("config() rhs must be atomic (not an expression or symbol)",
                   eq$source, source)
  }

  if (storage.mode(value) != expected_type) {
    ir_parse_error(sprintf(
      "Expected a %s for config(%s) but recieved a %s",
      expected_type, target, storage.mode(value)),
      eq$source, source)
  }

  eq
}
