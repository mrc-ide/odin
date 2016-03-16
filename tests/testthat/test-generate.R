context("generate")

test_that("array_dim_name", {
  expect_equal(array_dim_name("delay_foo", use=TRUE), "dim_foo")
  expect_null(array_dim_name("delay_foo", use=FALSE))
  expect_equal(array_dim_name("delay_foo_bar", use=TRUE), "dim_foo_bar")
  expect_null(array_dim_name("delay_foo_bar", use=FALSE))

  expect_equal(array_dim_name("delay_i_foo", use=TRUE), "dim_delay_foo")
  expect_equal(array_dim_name("delay_i_foo", use=FALSE), "dim_delay_foo")
  expect_equal(array_dim_name("delay_state_foo", use=TRUE), "dim_delay_foo")
  expect_null(array_dim_name("delay_state_foo", use=FALSE))
})
