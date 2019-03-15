context("generate: r (tools)")


## Because we put the scalar variables into the packing first, they
## never trigger the language branch of this function.  But that's
## something that could in theory change.
test_that("r_offset_to_position", {
  expect_equal(r_offset_to_position(0), 1)
  expect_equal(r_offset_to_position(10), 11)
  expect_equal(r_offset_to_position(quote(x)), quote(x + 1L))
  expect_equal(r_offset_to_position(quote(f(x))), quote(f(x) + 1L))
})
