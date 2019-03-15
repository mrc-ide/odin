ir_parse_config <- function(eqs, base_default, root, source) {
  i <- vcapply(eqs, "[[", "type") == "config"

  config <- lapply(unname(eqs[i]), ir_parse_config1, source)

  nms <- vcapply(config, function(x) x$lhs$name_data)

  base <- ir_parse_config_base(config[nms == "base"], base_default, source)
  include <- ir_parse_config_include(config[nms == "include"], root, source)

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


ir_parse_config_include <- function(config, root, source) {
  if (length(config) == 0L) {
    return(NULL)
  }

  read1 <- function(x) {
    filename <- file.path(root, x$rhs$value)
    if (!file.exists(filename)) {
      ir_parse_error(
        sprintf("Could not find file '%s' (relative to root '%s')",
                x$rhs$value, root),
        x$source, source)
    }
    tryCatch(
      read_user_c(filename),
      error = function(e)
        ir_parse_error(paste("Could not read include file:", e$message),
                       x$source, source))
  }

  ## TODO: this is more roundabout than needed - join_library should
  ## be replaced to support this.
  res <- join_library(lapply(config, read1))
  if (any(duplicated(res$declarations))) {
    ir_parse_error("Duplicate declarations while reading includes",
                   ir_parse_error_lines(config), source)
  }

  declarations <- strsplit(res$declarations, "\n")
  definitions <- strsplit(res$definitions, "\n")
  ret <- lapply(names(res$declarations), function(x)
    list(name = x,
         declaration = declarations[[x]],
         definition = definitions[[x]]))
  names(ret) <- names(res$declarations)
  ret
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
