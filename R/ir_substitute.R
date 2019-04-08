## ir_substitute and callees is used to rewrite expressions that use
## arrays within delay blocks
ir_substitute <- function(eqs, substitutions) {
  if (length(substitutions) == 0L) {
    return(eqs)
  }

  lapply(eqs, ir_substitute1, substitutions)
}


ir_substitute1 <- function(eq, substitutions) {
  from <- names(substitutions)
  if (any(from %in% eq$depends$variables)) {
    if (eq$type == "expression_array") {
      f <- function(x) {
        x$value <- ir_substitute_sexpr(x$value, substitutions)
        x
      }
      eq$rhs <- lapply(eq$rhs, f)
    } else {
      eq$rhs$value <- ir_substitute_sexpr(eq$rhs$value, substitutions)
    }
  }
  if (eq$lhs %in% from) {
    eq$lhs <- substitutions[[eq$lhs]]
  }
  eq
}


ir_substitute_sexpr <- function(expr, substitutions) {
  if (length(substitutions) == 0L) {
    expr
  } else if (is.recursive(expr)) {
    lapply(expr, ir_substitute_sexpr, substitutions)
  } else if (is.character(expr) && expr %in% names(substitutions)) {
    substitutions[[expr]]
  } else {
    expr
  }
}
