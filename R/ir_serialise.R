ir_serialise <- function(dat, pretty) {
  res <- list(version = ir_serialise_version(dat$version),
              config = ir_serialise_config(dat$config),
              meta = ir_serialise_meta(dat$meta),
              features = ir_serialise_features(dat$features),
              data = ir_serialise_data(dat$data),
              equations = ir_serialise_equations(dat$equations),
              components = ir_serialise_components(dat$components),
              user = ir_serialise_user(dat$user),
              interpolate = ir_serialise_interpolate(dat$interpolate),
              source = ir_serialise_source(dat$source))
  to_json(res, pretty)
}


ir_serialise_version <- function(version) {
  scalar(as.character(version))
}


ir_serialise_config <- function(config) {
  custom <- config$custom
  if (!is.null(config$custom)) {
    for (i in seq_along(custom)) {
      custom[[i]]$name <- scalar(custom[[i]]$name)
      custom[[i]]$value <- scalar(custom[[i]]$value)
    }
  }
  list(base = scalar(config$base),
       include = config$include,
       custom = custom)
}


ir_serialise_meta <- function(meta) {
  lapply(meta, scalar)
}


ir_serialise_features <- function(features) {
  lapply(features, scalar)
}


## The next four seem trivial
ir_serialise_components <- function(components) {
  components
}


ir_serialise_user <- function(user) {
  lapply(user, lapply, scalar)
}


ir_serialise_interpolate <- function(interpolate) {
  interpolate
}


ir_serialise_source <- function(source) {
  source
}


## These two are by far the most work
ir_serialise_data <- function(data) {
  f_elements <- function(x) {
    ret <- lapply(x[c("name", "location", "storage_type", "rank")], scalar)
    if (x$rank == 0L) {
      ret["dimnames"] <- list(NULL)
    } else {
      ret$dimnames <- x$dimnames
      ret$dimnames$length <- ir_serialise_expression(x$dimnames$length)
      ret$dimnames$dim <- lapply(ret$dimnames$dim, ir_serialise_expression)
      ret$dimnames$mult <- lapply(ret$dimnames$mult, ir_serialise_expression)
    }
    ret$stage <- scalar(STAGE_NAME[x$stage + 1L])
    ret
  }
  ## TODO: this can be modified later on when we move initial out of
  ## this place and put it in its own block.
  f_variable_contents <- function(x) {
    list(name = scalar(x$name),
         offset = ir_serialise_expression(x$offset),
         initial = scalar(x$initial))
  }
  f_output_contents <- function(x) {
    list(name = scalar(x$name),
         offset = ir_serialise_expression(x$offset))
  }

  elements <- lapply(unname(data$elements), f_elements)

  variable <- list(
    length = ir_serialise_expression(data$variable$length),
    contents = lapply(unname(data$variable$contents), f_variable_contents))
  output <- list(
    length = ir_serialise_expression(data$output$length),
    contents = lapply(unname(data$output$contents), f_output_contents))

  list(elements = elements,
       variable = variable,
       output = output)
}


ir_serialise_equations <- function(equations) {
  lapply(unname(equations), ir_serialise_equation)
}


ir_serialise_equation <- function(eq) {
  if (all(lengths(eq$depends) == 0L)) {
    depends <- NULL
  } else {
    depends <- eq$depends
  }
  base <- list(name = scalar(eq$name),
               type = scalar(eq$type),
               source = eq$source,
               depends = depends,
               lhs = scalar(eq$lhs$name_lhs))
  extra <- switch(
    eq$type,
    alloc = ir_serialise_equation_alloc(eq),
    alloc_interpolate = ir_serialise_equation_alloc_interpolate(eq),
    alloc_ring = ir_serialise_equation_alloc_ring(eq),
    copy = ir_serialise_equation_copy(eq),
    delay_continuous = ir_serialise_delay_continuous(eq),
    delay_discrete = ir_serialise_delay_discrete(eq),
    delay_index = ir_serialise_equation_delay_index(eq),
    expression_array = ir_serialise_equation_expression_array(eq),
    expression_scalar = ir_serialise_equation_expression_scalar(eq),
    expression_inplace = ir_serialise_equation_expression_inplace(eq),
    interpolate = ir_serialise_equation_interpolate(eq),
    user = ir_serialise_equation_user(eq),
    stop("odin bug"))
  c(base, extra)
}


ir_serialise_equation_alloc <- function(eq) {
  NULL
}


ir_serialise_equation_alloc_interpolate <- function(eq) {
  v <- c("t", "y", "type", "equation")
  list(interpolate = lapply(eq$interpolate[v], scalar))
}


ir_serialise_equation_alloc_ring <- function(eq) {
  list(delay = scalar(eq$delay))
}


ir_serialise_equation_copy <- function(eq) {
  NULL
}


ir_serialise_equation_expression_scalar <- function(eq) {
  list(rhs = list(value = ir_serialise_expression(eq$rhs$value)))
}


ir_serialise_equation_expression_inplace <- function(eq) {
  list(rhs = list(value = ir_serialise_expression(eq$rhs$value)))
}


ir_serialise_equation_expression_array <- function(eq) {
  rhs <- function(x) {
    index <- lapply(x$index, function(i)
      list(value = ir_serialise_expression(i$value),
           is_range = scalar(i$is_range),
           index = scalar(i$index)))
    list(index = index, value = ir_serialise_expression(x$value))
  }
  list(rhs = lapply(eq$rhs, rhs))
}

ir_serialise_equation_interpolate <- function(eq) {
  list(interpolate = scalar(eq$interpolate))
}

ir_serialise_equation_user <- function(eq) {
  list(user = list(default = ir_serialise_expression(eq$user$default),
                   dim = scalar(eq$user$dim),
                   min = ir_serialise_expression(eq$user$min),
                   max = ir_serialise_expression(eq$user$max)))
}


ir_serialise_delay_continuous <- function(eq) {
  f_contents <- function(x) {
    list(name = scalar(x$name),
         offset = ir_serialise_expression(x$offset))
  }
  variables <- list(
    length = ir_serialise_expression(eq$delay$variables$length),
    contents = lapply(unname(eq$delay$variables$contents), f_contents))
  substitutions <- lapply(eq$delay$substitutions, function(x)
    list(from = scalar(x$from), to = scalar(x$to)))
  rhs <- list(value = ir_serialise_expression(eq$rhs$value))
  if (!is.null(eq$rhs$index)) {
    rhs$index <- lapply(eq$rhs$index, function(i)
      list(value = ir_serialise_expression(i$value),
           is_range = scalar(i$is_range),
           index = scalar(i$index)))
  }

  list(rhs = rhs,
       delay = list(
         state = scalar(eq$delay$state),
         index = scalar(eq$delay$index),
         substitutions = substitutions,
         variables = variables,
         equations = eq$delay$equations,
         default = ir_serialise_expression(eq$delay$default),
         time = ir_serialise_expression(eq$delay$time)))
}


ir_serialise_equation_delay_index <- function(eq) {
  list(delay = scalar(eq$delay))
}


ir_serialise_delay_discrete <- function(eq) {
  ret <- list(rhs = list(value = ir_serialise_expression(eq$rhs$value)),
              delay = list(
                ring = scalar(eq$delay$ring),
                time = ir_serialise_expression(eq$delay$time),
                default = ir_serialise_expression(eq$delay$default)))
  if (!is.null(eq$rhs$index)) {
    ret$rhs$index <- lapply(eq$rhs$index, function(i)
      list(value = ir_serialise_expression(i$value),
           is_range = scalar(i$is_range),
           index = scalar(i$index)))
  }
  ret
}


ir_serialise_expression <- function(expr) {
  if (is.symbol(expr)) {
    jsonlite::unbox(as.character(expr))
  } else if (is.atomic(expr)) {
    jsonlite::unbox(expr)
  } else if (is.null(expr)) {
    NULL
  } else if (is.call(expr)) {
    c(list(jsonlite::unbox(as.character(expr[[1L]]))),
      lapply(expr[-1L], ir_serialise_expression))
  } else {
    stop("unhandled expression [odin bug]") # nocov
  }
}
