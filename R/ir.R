odin_ir <- function(x, type = NULL, validate = FALSE, pretty = TRUE) {
  ## TODO: see comments in odin validate model - this might want to
  ## flip around
  res <- odin_validate_model(x, type)

  dat <- res$result

  ir_dat <- list(config = ir_config(dat),
                 features = ir_features(dat),
                 equations = ir_equations(dat))
  ir <- jsonlite::toJSON(ir_dat, null = "null", pretty = pretty)
  if (validate) {
    ir_validate(ir, TRUE)
  }
  ir
}


ir_config <- function(dat) {
  ## We should probably add here:
  ##
  ## - odin version
  ## - file
  ## - type
  ## - path
  ## - include?
  list(base = jsonlite::unbox(dat$config$base))
}


ir_features <- function(dat) {
  v <- c("discrete", "has_array", "has_output", "has_user", "has_delay",
         "has_interpolate", "has_stochastic")
  lapply(dat$info[v], jsonlite::unbox)
}


ir_equations <- function(dat) {
  unname(lapply(dat$eqs, ir_equation))
}


ir_equation <- function(eq) {
  lhs <- list(data_type = jsonlite::unbox(eq$lhs$data_type))
  if (!is.null(lhs$special)) {
    lhs$special <- jsonlite::unbox(eq$lhs$special)
    lhs$target <- jsonlite::unbox(eq$lhs$target)
  }

  if (eq$rhs$type == "atomic") {
    rhs <- list(type = jsonlite::unbox(eq$rhs$type),
                value = jsonlite::unbox(eq$rhs$value))
  } else if (eq$rhs$type == "expression") {
    rhs <- list(type = jsonlite::unbox(eq$rhs$type),
                value = ir_expression(eq$rhs$value),
                depends = eq$depends)
  } else {
    stop("rhs type needs implementing")
  }

  list(name = jsonlite::unbox(eq$name),
       source = list(expression = jsonlite::unbox(eq$expr_str),
                     line = jsonlite::unbox(eq$line)),
       stage = jsonlite::unbox(STAGES[[eq$stage]]),
       stochastic = jsonlite::unbox(eq$stochastic),
       lhs = lhs,
       rhs = rhs)
}


ir_expression <- function(expr) {
  if (is.symbol(expr)) {
    jsonlite::unbox(as.character(expr))
  } else if (is.atomic(expr)) {
    jsonlite::unbox(expr)
  } else if (is.call(expr)) {
    c(list(jsonlite::unbox(as.character(expr[[1L]]))),
      lapply(expr[-1L], ir_expression))
  } else {
    stop("implement me")
  }
}


## Eventually this should be cached within a session, but wait until
## it works.
ir_schema <- function() {
  path <- system.file("schema.json", package = "odin", mustWork = TRUE)
  dat <- readChar(path, file.size(path))
  ## We get somewhat better errors from jsonlite's parsers than hoping
  ## that the json is valid.
  jsonlite::fromJSON(dat)
  dat
}


ir_validate <- function(x, error = FALSE) {
  engine <- if (packageVersion("jsonvalidate") > "1.0.0") "ajv" else "imjv"
  jsonvalidate::json_validate(x, ir_schema(),
                              verbose = TRUE, greedy = TRUE, error = error,
                              engine = "imjv")
}
