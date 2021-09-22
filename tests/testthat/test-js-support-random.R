context("support: random")

test_that("binomial", {
  test_binomial <- odin_js_test_random("binomial")

  ## known cases:
  expect_equal(test_binomial(100, c(100, 0)),
               rep(0, 100))
  expect_equal(test_binomial(100, c(100, 1)),
               rep(100, 100))

  ## rough validation of the mean (trying to confirm that we have the
  ## parameters correct).
  try_again(10, {
    res <- test_binomial(10000, c(10, 0.1))
    expect_equal(mean(res), 1, tolerance = 0.01)
    expect_equal(var(res), 0.9, tolerance = 0.01)
  })
})


test_that("exponential", {
  test_exponential <- odin_js_test_random("exponential")
  try_again(10, {
    res <- test_exponential(10000, 0.01)
    expect_equal(mean(res), 100, tolerance = 0.01)
    expect_equal(var(res), 10000, tolerance = 0.01)
  })
  try_again(10, {
    res <- test_exponential(10000, 100)
    expect_equal(mean(res), 0.01, tolerance = 0.01)
    expect_equal(var(res), 0.0001, tolerance = 0.01)
  })
})


test_that("geometric", {
  test_geometric <- odin_js_test_random("geometric")
  try_again(10, {
    res <- test_geometric(50000, 0.2)
    expect_equal(mean(res), 5, tolerance = 0.01)
    expect_equal(var(res), 20, tolerance = 0.01)
  })
})


test_that("normal", {
  test_normal <- odin_js_test_random("normal")

  try_again(10, {
    res <- test_normal(10000, c(0, 1))
    expect_equal(mean(res), 0, tolerance = 0.01)
    expect_equal(var(res), 1, tolerance = 0.01)
  })

  try_again(10, {
    res <- test_normal(10000, c(3, 10))
    expect_equal(mean(res), 3, tolerance = 0.04)
    expect_equal(var(res), 100, tolerance = 0.01)
  })
})


test_that("poisson", {
  test_poisson <- odin_js_test_random("poisson")

  try_again(10, {
    res <- test_poisson(10000, 1)
    expect_equal(mean(res), 1, tolerance = 0.01)
    expect_equal(var(res), 1, tolerance = 0.01)
  })

  try_again(10, {
    res <- test_poisson(10000, 3)
    expect_equal(mean(res), 3, tolerance = 0.01)
    expect_equal(var(res), 3, tolerance = 0.01)
  })
})


test_that("uniform", {
  test_uniform <- odin_js_test_random("uniform")

  res <- test_uniform(1000, c(2, 4))
  expect_true(all(res >= 2))
  expect_true(all(res <= 4))

  try_again(10, {
    res <- test_uniform(10000, c(2, 4))
    expect_equal(mean(res), 3, tolerance = 0.01)
    expect_equal(var(res), 1/3, tolerance = 0.01)
  })
})


test_that("set seed", {
  skip_if_no_random_js()
  v8 <- V8::v8()
  v8$eval(package_js("random.js"))

  v8$call("setSeed", "hello")
  a <- v8$call("random.unifRand")
  b <- v8$call("random.unifRand")

  v8$call("setSeed", "hello")
  expect_equal(v8$call("random.unifRand"), a)
  expect_equal(v8$call("random.unifRand"), b)
})
