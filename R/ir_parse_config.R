ir_parse_config <- function(eqs, base_default, root, source,
                            read_include, custom) {
  i <- vcapply(eqs, "[[", "type") == "config"

  config <- lapply(unname(eqs[i]), ir_parse_config1, source, custom)

  nms <- vcapply(config, function(x) x$lhs$name_data)

  base <- ir_parse_config_base(config[nms == "base"], base_default, source)
  include <- ir_parse_config_include(config[nms == "include"], root, source,
                                     read_include)
  custom <- ir_parse_config_custom(config[nms %in% custom], source)

  ret <- list(base = base, include = include)
  if (length(custom) > 0) {
    ret$custom <- custom
  }
  ret
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


ir_parse_config_custom <- function(x, source) {
  if (length(x) == 0) {
    return(NULL)
  }

  ## Is there any other validation that can really be done? We could
  ## require that custom cases conform to particular types or are
  ## unique? For now we'll be really leniant since we don't document
  ## this as a public interface yet.
  name <- vcapply(x, function(el) el$lhs$name_lhs)
  value <- lapply(x, function(el) el$rhs$value)
  unname(Map(list, name = name, value = value))
}


ir_parse_config1 <- function(eq, source, custom) {
  target <- eq$lhs$name_data
  value <- eq$rhs$value

  expected_type <- switch(
    target,
    base = "character",
    include = "character",
    NULL)

  if (is.null(expected_type)) {
    if (!(target %in% custom)) {
      ir_parse_error(sprintf("Unknown configuration option: %s", target),
                     eq$source, source)
    }
  } else {
    if (storage.mode(value) != expected_type) {
      ir_parse_error(sprintf(
        "Expected a %s for config(%s) but recieved a %s",
        expected_type, target, storage.mode(value)),
        eq$source, source)
    }
  }

  eq
}
