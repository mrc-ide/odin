## Continuous distributions are easy:
expectation_continuous <- function(fd, pars, from, to) {
  integrate(
    function(x) x * do.call(fd, c(list(x), unname(pars))),
    from, to)$value
}


## Discrete distrbutions are somewhat harder
expectation_discrete <- function(fd, fq, pars, tol = 1e-12) {
  end <- do.call(fq, c(list(p = 1 - tol), unname(pars)))
  n <- seq(0, end, by = 1)
  sum(n * do.call(fd, c(list(n), unname(pars))))
}
