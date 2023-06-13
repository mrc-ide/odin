test_that("can rewrite expressions to make them deterministic", {
  expect_equal(
    make_deterministic(quote(rnorm(a, b))),
    quote(a))
  expect_equal(
    make_deterministic(quote(rnorm(a, b) + rexp(c) + rbinom(n, p))),
    quote(a + 1 / c + n * p))
})


test_that("expectations of std distrbutions (with no args) are correct", {
  expect_equal(make_deterministic(quote(unif_rand())), 0.5)
  expect_equal(make_deterministic(quote(norm_rand())), 0)
  expect_equal(make_deterministic(quote(exp_rand())), 1)
})


test_that("expectation of beta is correct", {
  expr <- make_deterministic(quote(rbeta(x, y)))
  expect_equal(expr, quote(x / (x + y)))
  pars <- list(x = 3, y = 5)
  expect_equal(
    eval(expr, pars),
    expectation_continuous(dbeta, pars, 0, 1))
})


test_that("expectation of binomial is correct", {
  expr <- make_deterministic(quote(rbinom(n, p)))
  expect_equal(expr, quote(n * p))
  pars <- list(n = 30, p = 0.212)
  expect_equal(
    eval(expr, pars),
    expectation_discrete(dbinom, qbinom, pars))
})


test_that("expectation of chisq is correct", {
  expr <- make_deterministic(quote(rchisq(h)))
  expect_equal(expr, quote(h))
  pars <- list(h = 3)
  expect_equal(
    eval(expr, pars),
    expectation_continuous(dchisq, pars, 0, Inf))
})


test_that("expectation of exponential is correct", {
  expr <- make_deterministic(quote(rexp(r)))
  expect_equal(expr, quote(1 / r))
  pars <- list(r = 6.234)
  expect_equal(
    eval(expr, pars),
    expectation_continuous(dexp, pars, 0, Inf))
})


test_that("expectation of f distribution is correct", {
  expr <- make_deterministic(quote(rf(a, b)))
  expect_equal(expr, quote(b / (b - 2)))
  pars <- list(a = 3, b = 5)
  expect_equal(
    eval(expr, pars),
    expectation_continuous(df, pars, 0, Inf))
})


test_that("expectation of gamma is correct", {
  expr <- make_deterministic(quote(rgamma(x, y)))
  expect_equal(expr, quote(x / y))
  pars <- list(x = 3, y = 5)
  expect_equal(
    eval(expr, pars),
    expectation_continuous(dgamma, pars, 0, Inf))
})


test_that("expectation of geometric is correct", {
  expr <- make_deterministic(quote(rgeom(pr)))
  expect_equal(expr, quote((1 - pr) / pr))
  pars <- list(pr = 1 / pi)
  expect_equal(
    eval(expr, pars),
    expectation_discrete(dgeom, qgeom, pars))
})


test_that("expectation of hypergeometric is correct", {
  expr <- make_deterministic(quote(rhyper(m, n, k)))
  expect_equal(expr, quote(k * m / (m + n)))
  pars <- list(k = 17, n = 42, m = 19)
  skip("error here to fix")
  expect_equal(
    eval(expr, pars),
    expectation_discrete(dhyper, qhyper, pars))
})


test_that("expectation of logistic is correct", {
  expr <- make_deterministic(quote(rlogis(a, b)))
  expect_equal(expr, quote(a))
  pars <- list(a = 3, b = 2)
  expect_equal(
    eval(expr, pars),
    expectation_continuous(dlogis, pars, -Inf, Inf))
})


test_that("expectation of lnorm is correct", {
  expr <- make_deterministic(quote(rlnorm(x, y)))
  expect_equal(expr, quote(exp(x + y^2/2)))
  pars <- list(x = 3, y = 0.25)
  expect_equal(
    eval(expr, pars),
    expectation_continuous(dlnorm, pars, 0, Inf))
})


test_that("expectation of norm is correct", {
  expr <- make_deterministic(quote(rnorm(x, y)))
  expect_equal(expr, quote(x))
  pars <- list(x = 3, y = 5)
  expect_equal(
    eval(expr, pars),
    expectation_continuous(dnorm, pars, -Inf, Inf))
})


test_that("expectation of negative binomial is correct", {
})


test_that("expectation of poisson is correct", {
  expr <- make_deterministic(quote(rpois(a)))
  expect_equal(expr, quote(a))
  pars <- list(a = pi)
  expect_equal(
    eval(expr, pars),
    expectation_discrete(dpois, qpois, pars))
})


test_that("expectation of t is correct", {
  expr <- make_deterministic(quote(rt(x)))
  expect_equal(expr, 0)
  pars <- list(x = 5)
  expect_equal(
    eval(expr, pars),
    expectation_continuous(dt, pars, -Inf, Inf))
})


test_that("expectation of weibull is correct", {
  expr <- make_deterministic(quote(rweibull(a, b)))
  expect_equal(expr, quote(b * gamma(1 + 1/a)))
  pars <- list(a = 2, b = pi)
  expect_equal(
    eval(expr, pars),
    expectation_continuous(dweibull, pars, -Inf, Inf))
})


test_that("expectation of wilcox is correct", {
  expr <- make_deterministic(quote(rwilcox(a, b)))
  expect_equal(expr, quote(a * b / 2))
  pars <- list(a = 5, b = 9)
  expect_equal(
    eval(expr, pars),
    expectation_discrete(dwilcox, qwilcox, pars))
})


test_that("expectation of signrank is correct", {
  expr <- make_deterministic(quote(rsignrank(a)))
  expect_equal(expr, quote(a * (a + 1) / 4))
  pars <- list(a = 5)
  expect_equal(
    eval(expr, pars),
    expectation_discrete(dsignrank, qsignrank, pars))
})


test_that("expectation of uniform is correct", {
  expr <- make_deterministic(quote(runif(x, y)))
  expect_equal(expr, quote((x + y) / 2))
  pars <- list(x = 3, y = 5)
  expect_equal(
    eval(expr, pars),
    expectation_continuous(dunif, pars, 3, 5))
  expect_equal(
    eval(expr, pars),
    expectation_continuous(dunif, pars, 0, 10),
    tolerance = 1e-6)
})


test_that("can't compute expectation of cauchy", {
  expect_error(
    make_deterministic(quote(rcauchy(x, y))),
    "The Cauchy distribution has no mean, and may not be used")
})
