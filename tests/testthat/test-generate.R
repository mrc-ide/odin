context("generate")

test_that("array_dim_name", {
  expect_equal(array_dim_name("delay_foo", use=TRUE), "dim_foo")
  expect_equal(array_dim_name("delay_foo", use=FALSE), "")
  expect_equal(array_dim_name("delay_foo_bar", use=TRUE), "dim_foo_bar")
  expect_equal(array_dim_name("delay_foo_bar", use=FALSE), "")

  ## This is overly complicated and is triggered only in the generate code.
  expect_equal(array_dim_name("delay_i_foo", use=TRUE), "dim_delay_foo")
  expect_equal(array_dim_name("delay_i_foo", use=FALSE), "dim_delay_foo")
  expect_equal(array_dim_name("delay_state_foo", use=TRUE), "dim_delay_foo")
  expect_equal(array_dim_name("delay_state_foo", use=FALSE), "")

  ## Test vectorisation of the above
  expect_equal(array_dim_name(c("delay_i_foo", "delay_state_foo"), use=TRUE),
               c("dim_delay_foo", "dim_delay_foo"))
  expect_equal(array_dim_name(c("delay_i_foo", "delay_state_foo"), use=FALSE),
               c("dim_delay_foo", ""))
})
