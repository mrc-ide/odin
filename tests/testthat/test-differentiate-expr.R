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

test_that("Can differentiate all the bits for the basic SIR model", {
  expect_equal(differentiate(quote(beta * I/N * dt), "dt"),
               quote(beta * I/N))
  expect_equal(differentiate(quote(1 - exp(-(gamma) * dt)), "dt"),
               quote(gamma * exp(-(gamma) * dt)))
  expect_equal(differentiate(quote(1 - exp(-gamma * dt)), "dt"),
               quote(gamma * exp(-gamma * dt)))
  ## One too many layers of parens:
  expect_equal(differentiate(quote(beta * I/N * dt), "N"),
               quote(-beta * I * dt/(N * N))) # TODO
  expect_equal(differentiate(quote(I + n_SI - n_IR), "n_IR"),
               -1)
  expect_equal(differentiate(quote(R + n_IR), "n_IR"),
               1)
  expect_equal(differentiate(quote(cases_cumul + n_SI), "n_SI"),
               1)
  expect_equal(differentiate(quote(if (step%%freq == 0L) n_SI else cases_inc + n_SI), "n_SI"),
               1)
  expect_equal(differentiate(quote(I + n_SI - n_IR), "n_SI"),
               1)
  expect_equal(differentiate(quote(S - n_SI), "n_SI"),
               -1)
  expect_equal(differentiate(quote(1L - exp(-p_inf)), "p_inf"),
               quote(exp(-p_inf)))
  expect_equal(differentiate(quote(I * p_IR), "p_IR"),
               quote(I))
  expect_equal(differentiate(quote(S * p_SI), "p_SI"),
               quote(S))
  expect_equal(differentiate(quote(cases_cumul + n_SI), "cases_cumul"),
               1)
  expect_equal(differentiate(quote(if (step%%freq == 0L) n_SI else cases_inc + n_SI), "cases_inc"),
               quote(if (step %% freq == 0L) 0 else 1))
  expect_equal(differentiate(quote(S + I + R), "I"),
               1)
  expect_equal(differentiate(quote(I * p_IR), "I"),
               quote(p_IR))
  ## There's some cancelling here, probably a better way of writing the quotient rule?
  expect_equal(differentiate(quote(beta * I/N * dt), "I"),
               quote(beta * dt/N))
  expect_equal(differentiate(quote(I + n_SI - n_IR), "I"),
               1)
  expect_equal(differentiate(quote(S + I + R), "R"),
               1)
  expect_equal(differentiate(quote(R + n_IR), "R"),
               1)
  expect_equal(differentiate(quote(S + I + R), "S"),
               1)
  expect_equal(differentiate(quote(S * p_SI), "S"),
               quote(p_SI))
  expect_equal(differentiate(quote(S - n_SI), "S"),
               1)
  expect_equal(differentiate(quote(beta * I/N * dt), "beta"),
               quote(I * dt/N))
  expect_equal(differentiate(quote(1L - exp(-gamma * dt)), "gamma"),
               quote(dt * exp(-gamma * dt)))
  ## Could simplify here in divide (TODO)
  expect_equal(differentiate(quote(cases_observed * log(cases_inc) - cases_inc - lfactorial(cases_observed)), "cases_inc"),
               quote(cases_observed/cases_inc - 1))
  expect_equal(differentiate(quote(I0), "I0"),
               1)
})


test_that("can construct expressions", {
  expect_equal(maths$times(2, quote((a * b))), quote(2 * a * b))
  expect_equal(maths$times(quote((a * b)), 2), quote(a * b * 2))
})


test_that("chains of multiplication are sorted canonically", {
  expect_equal(maths$minus(2, quote((a * b))), quote(2 * a * b))
  expect_equal(maths$times(quote((a * b)), 2), quote(a * b * 2))
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
