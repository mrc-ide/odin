odin_ir <- function(x, type = NULL, validate = FALSE, pretty = TRUE) {
  ## TODO: see comments in odin validate model - this might want to
  ## flip around
  res <- odin_validate_model(x, type)

  dat <- res$result

  ir_dat <- list(config = ir_config(dat),
                 features = ir_features(dat),
                 data = ir_data(dat),
                 variables = ir_variables(dat),
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
  if (dat$info$has_array && dat$info$has_user) {
    ## This is harder because it breaks the ordering code
    stop("check for and enforce user sized arrays")
  }
  lapply(dat$info[v], jsonlite::unbox)
}


ir_variables <- function(dat) {
  vinfo <- dat$variable_info
  info <- lapply(seq_along(vinfo$order), function(i)
    list(name = jsonlite::unbox(vinfo$order[[i]]),
         rank = jsonlite::unbox(vinfo$array[[i]]),
         length = jsonlite::unbox(vinfo$len[[i]]),
         offset = jsonlite::unbox(vinfo$offset[[i]])))
  names(info) <- vinfo$order
  list(length = jsonlite::unbox(vinfo$total),
       length_stage = jsonlite::unbox(STAGES[[vinfo$total_stage]]),
       order = vinfo$order,
       info = info)
}


ir_equations <- function(dat) {
  unname(lapply(dat$eqs, ir_equation))
}



ir_equation <- function(eq) {
  lhs <- list(data_type = jsonlite::unbox(eq$lhs$data_type))
  if (!is.null(eq$lhs$special)) {
    lhs$special <- jsonlite::unbox(eq$lhs$special)
    lhs$target <- jsonlite::unbox(eq$lhs$name_target)
  }

  ## This is the major classification of types: things really route
  ## through different routes later based on this.  Later on we'll
  ## push some of the logic here into the parse functions I think
  if (identical(eq$lhs$special, "dim")) {
    type <- "dim"
  } else if (isTRUE(eq$rhs$interpolate)) {
    type <- "interoplate"
  } else if (isTRUE(eq$rhs$delay)) {
    type <- "delay"
  } else {
    ## TODO: I think that user() *must* be special really?
    type <- "expression"
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
       type = jsonlite::unbox(type),
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


## TODO: This actually belongs in the parse stage I think
ir_data <- function(dat) {
  ret <- unname(lapply(dat$eqs, ir_data1))
  ret[!vlapply(ret, is.null)]
}


ir_data1 <- function(eq) {
  if (identical(eq$lhs$special, "initial") || eq$stage < STAGE_TIME) {
    if (eq$lhs$type != "symbol") {
      stop("FIXME")
    }
    list(name = jsonlite::unbox(eq$name),
         storage_type = jsonlite::unbox(eq$lhs$data_type),
         rank = jsonlite::unbox(0L))
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
