make_names2 <- function(variable_order, output_order, discrete) {
  make_names(c(variable_order, output_order), discrete)
}


support_n_out <- function(output_order) {
  n <- vnapply(output_order, function(x) if (is.null(x)) 1L else prod(x))
  as.integer(sum(n))
}


support_transform_variables <- function(y, variable_order, output_order,
                                        discrete) {
  ord <- list(variable_order = variable_order, output_order = output_order)
  make_transform_variables(ord)(y)
}
