test_that("can differentiate trivial expressions", {
  expect_equal(differentiate(quote(1), "x"), 0)
  expect_equal(differentiate(quote(y), "x"), 0)
  expect_equal(differentiate(quote(x), "x"), 1)
})

test_that("can apply the product rule", {
  expect_equal(differentiate(quote(x * 2), "x"), 2)
  expect_equal(differentiate(quote(2 * x), "x"), 2)
  expect_equal(differentiate(quote(x * y), "x"), quote(y))
  expect_equal(differentiate(quote(y * x), "x"), quote(y))
  expect_equal(differentiate(quote(2 * x * y), "x"), quote(2 * y))

  expect_equal(differentiate(quote((y) * x), "x"), quote(y))
  differentiate(quote(-(y) * x), "x") # TODO: simplify me
})

test_that("parentheses have no effect in product chains", {
  expect_equal(differentiate(quote(2 * (x) * y), "x"), quote(2 * y))
  expect_equal(differentiate(quote(2 * (x * y)), "x"), quote(2 * y))
  expect_equal(differentiate(quote((2 * (x) * y)), "x"), quote(2 * y))
})

test_that("can add derivatives", {
  expect_equal(differentiate(quote(x + 5), "x"), 1)
  expect_equal(differentiate(quote(5 + x), "x"), 1)
  expect_equal(differentiate(quote(x + y), "x"), 1)
  expect_equal(differentiate(quote(y + x), "x"), 1)
  expect_equal(differentiate(quote(5 + x + x), "x"), quote(2))
})

test_that("can subtract derivatives", {
  expect_equal(differentiate(quote(x - y), "x"), 1)
  expect_equal(differentiate(quote(y - x), "x"), -1)
  expect_equal(differentiate(quote(2 * x - exp(x)), "x"), quote(2 - exp(x)))
})

test_that("quotient rule is correct", {
  expect_equal(
    differentiate(quote(a / b), "a"),
    quote(1 / b))
  expect_equal(differentiate(quote(exp(x) / x), "x"),
               quote(exp(x) / x - exp(x) / (x * x)))
})

test_that("can differentiate f(x)^a", {
  expect_equal(differentiate(quote(x^a), "x"),
               quote(a * x^(a - 1)))
  expect_equal(differentiate(quote((2 * x)^a), "x"),
               quote(2 * a * (2 * x)^(a - 1)))
  expect_equal(differentiate(quote(x^a), "a"),
               quote(x^a * log(x)))
  expect_equal(differentiate(quote(x^(2 * a)), "a"),
               quote(2 * x^(2 * a) * log(x)))

  ## d/dx (f(x)^g(x)) is much more complicated:
  expect_equal(
    differentiate(quote((2 * x)^exp(x)), "x"),
    quote((2 * x)^(exp(x) - 1) * (2 * exp(x) + 2 * x * log(2 * x) * exp(x))))
  expect_equal(
    eval(differentiate(quote((2 * x)^exp(x)), "x"), list(x = 0.3)),
    eval(D(quote((2 * x)^exp(x)), "x"), list(x = 0.3)))
})


test_that("differentiate expressions with exp()", {
  expect_equal(differentiate(quote(exp(x)), "x"),
               quote(exp(x)))
  expect_equal(differentiate(quote(exp(2 * x)), "x"),
               quote(2 * exp(2 * x)))
  expect_equal(differentiate(quote(exp((2 * x))), "x"),
               quote(2 * exp(2 * x)))
})


test_that("differentiate expressions with log()", {
  expect_equal(
    differentiate(quote(log(x)), "x"),
    quote(1 / x))
  expect_equal(
    differentiate(quote(log(2 * x)), "x"),
    quote(2 / (2 * x)))
  expect_equal(
    differentiate(quote(a * log(x) - x), "x"),
    quote(a / x - 1))
})

test_that("differentiate conditionals", {
  expect_equal(
    differentiate(quote(if (a) x else 2 * x), "x"),
    quote(if (a) 1 else 2))
  expect_equal(
    differentiate(quote(if (a) x else x), "x"),
    1)
  expect_equal(
    differentiate(quote(if (a) 2 * x else 2 * x), "x"),
    2)
})


test_that("can construct expressions", {
  expect_equal(maths$times(2, quote((a * b))), quote(2 * a * b))
  expect_equal(maths$times(quote((a * b)), 2), quote(2 * a * b))
})

test_that("chains of multiplication are sorted canonically", {
  expect_equal(maths$times(2, quote((a * b))), quote(2 * a * b))
  expect_equal(maths$times(quote((a * b)), 2), quote(2 * a * b))
  expect_equal(maths$times(maths$times(2, quote(a)), maths$times(quote(b), 3)),
               quote(6 * a * b))
})


test_that("multiplication strips excess parentheses", {
  expect_equal(maths$times(quote((a)), 1), quote(a))
  expect_equal(maths$times(quote((a)), quote(b)), quote(a * b))
})


test_that("simplify multiply-and-divide", {
  expect_equal(maths$times(quote(a / b), quote(c)),
               quote(a * c / b))
  expect_equal(maths$times(quote(a * b / c), quote(d)),
               quote(a * b * d / c))
})


test_that("simplify repeated-divide", {
  expect_equal(maths$divide(quote(a / b), quote(c)),
               quote(a / (b * c)))
  expect_equal(maths$times(quote(a * b / c), quote(d)),
               quote(a * b * d / c))
})


test_that("can move unary minuses through product chains", {
  expect_equal(maths$times(quote(a), quote(b)), quote(a * b))
  expect_equal(maths$times(quote(-a), quote(b)), quote(-a * b))
  expect_equal(maths$times(quote(a), quote(-b)), quote(-a * b))
  expect_equal(maths$times(quote(-a), quote(-b)), quote(a * b))
  expect_equal(maths$times(maths$times(quote(a), quote(b)), quote(-c)),
               quote(-a * b * c))
  expect_equal(maths$times(maths$times(quote(a), quote(b)),
                           maths$times(quote(c), quote(-d))),
               quote(-a * b * c * d))
})


test_that("can move unary minuses through divisions chains", {
  expect_equal(maths$divide(quote(a), quote(b)), quote(a / b))
  expect_equal(maths$divide(quote(-a), quote(b)), quote(-a / b))
  expect_equal(maths$divide(quote(a), quote(-b)), quote(-a / b))
  expect_equal(maths$divide(quote(-a), quote(-b)), quote(a / b))
  expect_equal(maths$divide(maths$divide(quote(a), quote(b)), quote(-c)),
               quote(-a / (b * c)))
  expect_equal(maths$divide(maths$divide(quote(a), quote(b)),
                            maths$divide(quote(c), quote(-d))),
               quote(-a * d / (b * c)))
})


test_that("can rewrite expressions", {
  expect_equal(maths$rewrite(quote(1 / x * a)), quote(a / x))
  expect_equal(maths$rewrite(quote(((b)) + (a))), quote(b + a))
  expect_equal(maths$rewrite(quote((a + b) * c)), quote((a + b) * c))
})


test_that("uminus strips parentheses", {
  expect_equal(maths$uminus(quote(a)), quote(-a))
  expect_equal(maths$uminus(quote((a))), quote(-a))
})
