context("can compile")

test_that("can_compile", {
  skip_on_cran()
  expect_true(can_compile())
  expect_true(can_compile())
  expect_true(can_compile(refresh = TRUE))
})


test_that("target", {
  skip_on_cran()
  oo <- options(odin.target = NULL)
  on.exit({
    .odin$can_compile <- NULL
    options(oo)
  })

  .odin$can_compile <- NULL
  expect_equal(odin_options(target = NULL)$target, "c")
  expect_equal(odin_options(target = "c")$target, "c")
  expect_equal(odin_options(target = "r")$target, "r")

  .odin$can_compile <- FALSE
  expect_equal(odin_options(target = NULL)$target, "r")
  expect_equal(odin_options(target = "r")$target, "r")
  expect_equal(odin_options(target = "c")$target, "c")

  .odin$can_compile <- TRUE
  expect_equal(odin_options(target = NULL)$target, "c")
  expect_equal(odin_options(target = "r")$target, "r")
  expect_equal(odin_options(target = "c")$target, "c")
})
