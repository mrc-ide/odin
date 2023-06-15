test_that("can differentiate trivial expressions", {
  expect_equal(differentiate(quote(1), "x"), 0)
  expect_equal(differentiate(quote(y), "x"), 0)
  expect_equal(differentiate(quote(x), "x"), 1)
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

test_that("quotient rule is correct", {
  expect_equal(
    differentiate(quote(a / b), "a"),
    quote(1 / b))
  expect_equal(differentiate(quote(a / b), "b"),
               quote(-a / (b * b)))
  expect_equal(differentiate(quote(exp(x) / x), "x"),
               quote(exp(x) / x - exp(x) / (x * x)))
})

test_that("can differentiate f(x)^g(x)", {
  expect_equal(differentiate(quote(x^a), "x"),
               quote(a * x^(a - 1)))
  expect_equal(differentiate(quote((2 * x)^a), "x"),
               quote(2 * a * (2 * x)^(a - 1)))
  expect_equal(differentiate(quote(x^a), "a"),
               quote(x^a * log(x)))
  expect_equal(differentiate(quote(x^(2 * a)), "a"),
               quote(2 * x^(2 * a) * log(x)))

  ## the full d/dx (f(x)^g(x)) is much more complicated:
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

test_that("differentiate square roots", {
  expect_equal(differentiate(quote(sqrt(x)), "x"),
               quote(1 / (2 * sqrt(x))))
  expect_equal(differentiate(quote(sqrt(1 + exp(x))), "x"),
               quote(exp(x) / (2 * sqrt(1 + exp(x)))))
})

test_that("differentiate conditionals", {
  expect_equal(differentiate(quote(if (a) x else 2 * x), "x"),
               quote(if (a) 1 else 2))
  expect_equal(differentiate(quote(if (a) x else x), "x"),
               1)
  expect_equal(differentiate(quote(if (a) 2 * x else 2 * x), "x"),
               2)
})

test_that("differentiate log factorial", {
  expect_equal(differentiate(quote(lfactorial(x)), "x"),
               quote(digamma(1 + x)))
  expect_equal(differentiate(quote(lfactorial(log(x))), "x"),
               quote(digamma(1 + log(x)) / x))
})

test_that("differentiate absolute value function", {
  expect_equal(differentiate(quote(abs(x)), "x"),
               quote(sign(x)))
  expect_equal(differentiate(quote(abs(x^2 - 2 * x - 1)), "x"),
               quote((2 * x - 2) * sign(x^2 - 2 * x - 1)))
})

test_that("error if asked to differentiate something not yet supported", {
  expect_error(
    differentiate(quote(f(x)), "x"),
    "Unsupported function 'f' in differentiate()",
    fixed = TRUE)
  expect_error(
    differentiate(quote(exp(2 * f(x))), "x"),
    "Unsupported function 'f' in differentiate()",
    fixed = TRUE)
})

test_that("can construct expressions", {
  expect_equal(maths$times(2, quote((a * b))), quote(2 * a * b))
  expect_equal(maths$times(quote((a * b)), 2), quote(2 * a * b))
})


test_that("plus copes with numeric edge cases", {
  expect_equal(maths$plus(1, 3), 4)
  expect_equal(maths$plus(0, quote(a)), quote(a))
  expect_equal(maths$plus(quote(a), 0), quote(a))
  expect_equal(maths$plus(0, quote((a))), quote(a))
  expect_equal(maths$plus(quote(((a + b))), 0), quote(a + b))
})


test_that("plus builds expressions if they can't be simplified", {
  expect_equal(maths$plus(quote(a), quote(b)), quote(a + b))
  expect_equal(maths$plus(quote(a), quote(1 + b)), quote(1 + a + b))
})


test_that("plus shifts numbers to the front and adds them up", {
  expect_equal(
    maths$plus(maths$plus(quote(a), quote(b)), maths$plus(1, quote(c))),
    quote(1 + a + b + c))
  expect_equal(
    maths$plus(maths$plus(quote(a), 3), maths$plus(1, quote(c))),
    quote(4 + a + c))
  expect_equal(
    maths$plus(maths$plus(quote(a), 3), maths$plus(quote(c), 1)),
    quote(4 + a + c))
})


test_that("subtraction copes with numeric edge cases", {
  expect_equal(maths$minus(10, 3), 7)
  expect_equal(maths$minus(quote(a), 0), quote(a))
  expect_equal(maths$minus(quote((a)), 0), quote(a))
  expect_equal(maths$minus(quote(a - b), 0), quote(a - b))
  expect_equal(maths$minus(0, quote(a)), quote(-a))
  expect_equal(maths$minus(0, quote(-a)), quote(a))
  expect_equal(maths$minus(0, quote(b - a)), quote(a - b))
})


test_that("subtraction appropriately protects second argument", {
  expect_equal(maths$minus(quote(a), quote(b)), quote(a - b))
  expect_equal(maths$minus(quote(a), quote(b + c)),
               quote(a - (b + c)))
  expect_equal(maths$minus(quote(a), quote(b + c))[[3]],
               quote((b + c)))
  ## could also simplify maths$minus(quote(a), quote(b - c)) to cancel
  ## the double minus?
})

test_that("uminus copes with numbers", {
  expect_equal(maths$uminus(4), -4)
  expect_equal(maths$uminus(-4), 4)
})


test_that("uminus strips parentheses", {
  expect_equal(maths$uminus(quote(a)), quote(-a))
  expect_equal(maths$uminus(quote((a))), quote(-a))
})


test_that("uminus shifts into first argument of product", {
  expect_equal(maths$uminus(quote(a * b)), quote(-a * b))
  expect_equal(maths$uminus(quote(-a * b)), quote(a * b))
  expect_equal(maths$uminus(quote(a * -b)), quote(a * b))
  expect_equal(maths$uminus(quote(-a * -b)), quote(-a * b))
})


test_that("uminus on fraction moves to numerator", {
  expect_equal(maths$uminus(quote(a / b)),
               quote(-a / b))
  expect_equal(maths$uminus(quote(a / (b * c))),
               quote(-a / (b * c)))
  expect_equal(maths$uminus(quote((a * b) / (c * d))),
               quote(-(a * b) / (c * d)))
  expect_equal(maths$uminus(quote(a * b / (c * d))),
               quote(-a * b / (c * d)))
})


test_that("uminus on subtraction reverses arguments", {
  expect_equal(maths$uminus(quote(a - b)),
               quote(b - a))
})


test_that("times copes with numeric edge cases", {
  expect_equal(maths$times(3, 5), 15)
  expect_equal(maths$times(1, quote(a)), quote(a))
  expect_equal(maths$times(quote(a), 1), quote(a))
  expect_equal(maths$times(0, quote(a)), 0)
  expect_equal(maths$times(quote(a), 0), 0)
  expect_equal(maths$times(-1, quote(a)), quote(-a))
  expect_equal(maths$times(quote(a), -1), quote(-a))
})


test_that("shift divisions down the multiplication chain", {
  expect_equal(maths$times(quote(a / b), quote(c)),
               quote(a * c / b))
  expect_equal(maths$times(quote(a / b), quote(c * d)),
               quote(a * c * d / b))
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


test_that("chains of multiplication collect numbers", {
  expect_equal(maths$times(2, quote((a * b))), quote(2 * a * b))
  expect_equal(maths$times(quote((a * b)), 2), quote(2 * a * b))
  expect_equal(maths$times(maths$times(2, quote(a)), maths$times(quote(b), 3)),
               quote(6 * a * b))
})


test_that("multiplication strips excess parentheses", {
  expect_equal(maths$times(quote((a)), 1), quote(a))
  expect_equal(maths$times(quote((a)), quote(b)), quote(a * b))
})


test_that("cope with division corner cases", {
  expect_equal(maths$divide(3, 4), 3 / 4)
  expect_equal(maths$divide(quote(a), 1), quote(a))
  expect_equal(maths$divide(0, quote(a)), 0)
  expect_equal(maths$divide(quote(a), 0), Inf)
})


test_that("simplify chains of divide", {
  expect_equal(maths$divide(quote(a / b), quote(c)),
               quote(a / (b * c)))
  expect_equal(maths$divide(quote(a), quote(b / c)),
               quote(a * c / b))
  expect_equal(maths$divide(quote(a / b), quote(c / d)),
               quote(a * d / (b * c)))
  expect_equal(maths$times(quote(a * b / c), quote(d)),
               quote(a * b * d / c))
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


test_that("pow copes with numeric edge cases", {
  expect_equal(maths$pow(3, 2), 9)
  expect_equal(maths$pow(quote(a), 1), quote(a))
})


test_that("rewrite squares of symbols", {
  expect_equal(maths$pow(quote(a), 2), quote(a * a))
  expect_equal(maths$pow(quote(a + b), 2), quote((a + b)^2))
})


test_that("protect arguments to pow", {
  expect_identical(maths$pow(quote(a + b), quote(c + d)),
                   quote((a + b)^(c + d)))
})


test_that("can rewrite expressions", {
  expect_equal(maths$rewrite(quote(1 / x * a)), quote(a / x))
  expect_equal(maths$rewrite(quote(((b)) + (a))), quote(b + a))
  expect_equal(maths$rewrite(quote((a + b) * c)), quote((a + b) * c))
})
