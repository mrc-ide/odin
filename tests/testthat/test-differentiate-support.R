test_that("can rewrite expressions to make them deterministic", {
  expect_equal(make_deterministic(quote(20)), quote(20))
  expect_equal(make_deterministic(quote(a + b)), quote(a + b))
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
  pars <- list(m = 19, n = 42, k = 17)
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
  expect_equal(expr, quote(exp(x + y^2 / 2)))
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
  expr <- make_deterministic(quote(rnbinom(n, p)))
  expect_equal(expr, quote(n * (1 - p) / p))
  pars <- list(n = 12, p = 0.234)
  expect_equal(
    eval(expr, pars),
    expectation_discrete(dnbinom, qnbinom, pars))
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
  expect_equal(expr, quote(b * gamma(1 + 1 / a)))
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


test_that("log density of normal is correct", {
  expr <- log_density("normal", quote(d), list(quote(a), quote(b)))
  expect_equal(expr, quote(-(d - a)^2 / (2 * b^2) - log(2 * pi) / 2 - log(b)))
  dat <- list(d = 2.341, a = 5.924, b = 4.2)
  expect_equal(eval(expr, dat),
               dnorm(dat$d, dat$a, dat$b, log = TRUE))
})


test_that("log density of poisson is correct", {
  expr <- log_density("poisson", quote(d), list(quote(mu)))
  expect_equal(expr, quote(d * log(mu) - mu - lfactorial(d)))
  dat <- list(d = 3, mu = 5.234)
  expect_equal(eval(expr, dat),
               dpois(dat$d, dat$mu, log = TRUE))
})


test_that("log density of uniform is correct", {
  expr <- log_density("uniform", quote(d), list(quote(x0), quote(x1)))
  expect_equal(expr, quote(if (d < x0 || d > x1) -Inf else -log(x1 - x0)))
  dat1 <- list(d = 3, x0 = 1, x1 = 75)
  expect_equal(eval(expr, dat1),
               dunif(dat1$d, dat1$x0, dat1$x1, log = TRUE))
  dat2 <- list(d = 3, x0 = 9, x1 = 75)
  expect_equal(eval(expr, dat2),
               dunif(dat2$d, dat2$x0, dat2$x1, log = TRUE))
  dat3 <- list(d = 3, x0 = 1, x1 = 2)
  expect_equal(eval(expr, dat3),
               dunif(dat3$d, dat3$x0, dat3$x1, log = TRUE))
})


test_that("disable unknown distributions", {
  expect_error(
    log_density("cauchy", quote(d), list(quote(a), quote(b))),
    "Unsupported distribution 'cauchy'")
})
