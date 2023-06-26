## Continuous distributions are easy:
expectation_continuous <- function(fd, pars, from, to) {
  integrate(
    function(x) x * do.call(fd, c(list(x), unname(pars))),
    from, to)$value
}


## Discrete distrbutions are somewhat harder. Take fd (the density 'd'
## function, e.g. dbinom) and fq (the corresponding quantile 'q'
## function, e.g., qbinom) and work out some suitably far out value
## that we capture at least 1-tol of the probability mass, then sum
## over that. This is not quite an infinite sum but at tolerance of
## 1e-12 we're around the limits of what we'd get summing over many
## floating point numbers (and this is only used in tests with a
## looser tolerance anyway)
expectation_discrete <- function(fd, fq, pars, tol = 1e-12) {
  end <- do.call(fq, c(list(p = 1 - tol), unname(pars)))
  n <- seq(0, end, by = 1)
  sum(n * do.call(fd, c(list(n), unname(pars))))
}
