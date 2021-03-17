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


test_that("sort expressions", {
  expect_equal(
    static_eval(quote(a + 1 + b + 2)),
    quote(a + b + 3))
  expect_equal(
    static_eval(quote(1 + b + a + 2)),
    quote(a + b + 3))
  expect_equal(
    static_eval(quote(1 + b + a + 2 + x * y)),
    quote(x * y + a + b + 3))
})


test_that("Addition of zero is a noop", {
  expect_equal(static_eval(quote(a + 0)), quote(a))
  expect_equal(static_eval(quote(a + 0 + b)), quote(a + b))
})


test_that("Multiplication by one is a noop", {
  expect_equal(static_eval(quote(a * 1)), quote(a))
  expect_equal(static_eval(quote(a * 1 * b)), quote(a * b))
})


test_that("Multiplication by zero is catatrophic", {
  expect_equal(static_eval(quote(a * 0)), 0)
  expect_equal(static_eval(quote(a * 0 * b)), 0)
})


test_that("Can evaluate very long expressions", {
  v <- sprintf("x%d", seq_len(200))
  e <- parse(text = paste(v, collapse = " + "))[[1]]
  expect_equal(
    static_eval(e),
    r_fold_call("+", lapply(sort(v), as.name)))
})


test_that("Can collect linear combinations", {
  expect_equal(
    static_eval(quote(a + b + a + b + a + 4)),
    quote(a * 3 + b * 2 + 4))
  ## This is something to pick up later
  expect_equal(
    static_eval(quote(a + 1 * (a + a))),
    quote(a * 2 + a))
})


test_that("cope with adding zeros", {
  expect_equal(
    static_eval(quote(0 + 0)),
    0)
  expect_equal(
    static_eval(quote(0 * x + 1 * 0)),
    0)
})
