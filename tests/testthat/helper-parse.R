## When creating test cases for the parser, we need to create model
## systems that include initial() and deriv()/update() calls
ex <- function(x, discrete = FALSE, var = "q") {
  rhs <- if (discrete) "update" else "deriv"
  sprintf("%s\ninitial(%s) <- 1\n%s(%s) <- 1", x, var, rhs, var)
}
