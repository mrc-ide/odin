## TODO: we should be able to see in the schema all the cases that are
## character vectors.
ir_deserialise <- function(ir) {
  dat <- jsonlite::fromJSON(ir, simplifyVector = FALSE)
  dat$components <- lapply(dat$components, lapply, list_to_character)

  if (dat$features$has_array) {
    dat$data$elements <- lapply(dat$data$elements, ir_deserialise_data_dimnames)
  }

  names(dat$data$elements) <- vcapply(dat$data$elements, "[[", "name")
  names(dat$data$variable$contents) <-
    vcapply(dat$data$variable$contents, "[[", "name")
  names(dat$data$output$contents) <-
    vcapply(dat$data$output$contents, "[[", "name")
  names(dat$equations) <- vcapply(dat$equations, "[[", "name")

  dat$interpolate <- lapply(dat$interpolate, list_to_character)
  dat$equations <- lapply(dat$equations, ir_deserialise_equation)
  dat$ir <- ir

  dat
}


ir_deserialise_equation <- function(eq) {
  if (!is.null(eq$depends)) {
    eq$depends <- lapply(eq$depends, list_to_character)
  }
  if (eq$type == "delay_continuous") {
    eq$delay$equations <- list_to_character(eq$delay$equations)
    names(eq$delay$variables$contents) <-
      vcapply(eq$delay$variables$contents, "[[", "name")
    eq$delay$substitutions <-
      set_names(
        vcapply(eq$delay$substitutions, "[[", "to"),
        vcapply(eq$delay$substitutions, "[[", "from"))
  }
  eq
}


ir_deserialise_data_dimnames <- function(x) {
  if (x$rank > 0L) {
    v <- c("dim", "mult")
    x$dimnames[v] <- lapply(x$dimnames[v], list_to_character)
  }
  x
}


ir_serialise <- function(dat, pretty) {
  res <- list(config = ir_serialise_config(dat$config),
              meta = ir_serialise_meta(dat$meta),
              features = ir_serialise_features(dat$features),
              data = ir_serialise_data(dat$data),
              equations = ir_serialise_equations(dat$equations),
              components = ir_serialise_components(dat$components),
              user = ir_serialise_user(dat$user),
              interpolate = ir_serialise_interpolate(dat$interpolate),
              source = ir_serialise_source(dat$source))
  ir_to_json(res, pretty)
}


ir_serialise_config <- function(config) {
  list(base = scalar(config$base))
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
      ret$dimnames$length <- ir_expression(x$dimnames$length)
    }
    ret
  }
  ## TODO: this can be modified later on when we move initial out of
  ## this place and put it in its own block.
  f_variable_contents <- function(x) {
    list(name = scalar(x$name),
         offset = ir_expression(x$offset),
         initial = scalar(x$initial))
  }
  f_output_contents <- function(x) {
    list(name = scalar(x$name),
         offset = ir_expression(x$offset))
  }

  elements <- lapply(unname(data$elements), f_elements)

  variable <- list(
    length = ir_expression(data$variable$length),
    contents = lapply(unname(data$variable$contents), f_variable_contents))
  output <- list(
    length = ir_expression(data$output$length),
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
  list(rhs = list(value = ir_expression(eq$rhs$value)))
}


ir_serialise_equation_expression_array <- function(eq) {
  rhs <- function(x) {
    index <- lapply(x$index, function(i)
      list(value = ir_expression(i$value),
           is_range = scalar(i$is_range),
           index = scalar(i$index)))
    list(index = index, value = ir_expression(x$value))
  }
  list(rhs = lapply(eq$rhs, rhs))
}


ir_serialise_equation_user <- function(eq) {
  list(user = list(default = ir_expression(eq$user$default),
                   dim = scalar(eq$user$dim),
                   min = ir_expression(eq$user$min),
                   max = ir_expression(eq$user$max)))
}


ir_serialise_delay_continuous <- function(eq) {
  f_contents <- function(x) {
    list(name = scalar(x$name),
         offset = ir_expression(x$offset))
  }
  variables <- list(
    length = ir_expression(eq$delay$variables$length),
    contents = lapply(unname(eq$delay$variables$contents), f_contents))
  substitutions <- lapply(eq$delay$substitutions, function(x)
    list(from = scalar(x$from), to = scalar(x$to)))
  rhs <- list(value = ir_expression(eq$rhs$value))
  if (!is.null(eq$rhs$index)) {
    rhs$index <- lapply(eq$rhs$index, function(i)
      list(value = ir_expression(i$value),
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
         default = ir_expression(eq$delay$default),
         time = ir_expression(eq$delay$time)))
}


ir_serialise_equation_delay_index <- function(eq) {
  list(delay = scalar(eq$delay))
}


ir_serialise_delay_discrete <- function(eq) {
  ret <- list(rhs = list(value = ir_expression(eq$rhs$value)),
              delay = list(ring = scalar(eq$delay$ring),
                           time = ir_expression(eq$delay$time),
                           default = ir_expression(eq$delay$default)))
  if (!is.null(eq$rhs$index)) {
    ret$rhs$index <- lapply(eq$rhs$index, function(i)
      list(value = ir_expression(i$value),
           is_range = scalar(i$is_range),
           index = scalar(i$index)))
  }
  ret
}


ir_to_json <- function(dat, pretty = TRUE) {
  jsonlite::toJSON(dat, null = "null", pretty = pretty, digits = NA)
}
