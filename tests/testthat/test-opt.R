context("opt")

test_that("static_eval completely evaluates numeric expressions", {
  expect_equal(static_eval(quote(1 + 2)), 3)
  expect_equal(static_eval(quote(1 + 2 + 3)), 6)
  expect_equal(static_eval(quote(1 + 2 * 3)), 7)
  expect_equal(static_eval(quote(1 + (2) + 3)), 6)
  expect_equal(static_eval(quote((1 + 2) * 3)), 9)
})


test_that("static_eval collects numbers up associatively", {
  expect_equal(static_eval(quote(a + 3 + 2)), quote(a + 5))
  expect_equal(static_eval(quote(3 + a + 2)), quote(a + 5))
  expect_equal(static_eval(quote(3 + 2 + a)), quote(a + 5))

  expect_equal(static_eval(quote(a * 3 * 2)), quote(a * 6))
  expect_equal(static_eval(quote(3 * a * 2)), quote(a * 6))
  expect_equal(static_eval(quote(3 * 2 * a)), quote(a * 6))

  expect_equal(static_eval(quote(a + 1 + b + 2 + c + 3)),
               quote(a + b + c + 6))
})


test_that("collect_assoc unfolds expressions", {
  expect_equal(collect_assoc(quote(a + b + c), quote(`+`)),
               list(quote(`+`), quote(a), quote(b), quote(c)))
  expect_equal(collect_assoc(quote(a + 1 + b + 2 + c + 3), quote(`+`)),
               list(quote(`+`), quote(a), 1, quote(b), 2, quote(c), 3))
})


test_that("static_eval removes superfluous parens", {
  expect_equal(static_eval(quote(1 + (a + 2))), quote(a + 3))
  expect_equal(static_eval(quote(1 + (a + 2) + 3)), quote(a + 6))
})


test_that("More complex examples", {
  expect_equal(static_eval(quote((a + 2 * 3) + 4 * 5)),
               quote(a + 26))
  expect_equal(static_eval(quote((a + 2 * 3) + 4 * b)),
               quote(b * 4 + a + 6))
  expect_equal(static_eval(quote((1 + 4) * (b + 3))),
               quote((b + 3) * 5))
})
