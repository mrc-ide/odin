test_that_odin("print unconditional debugging", {
  skip_for_target("js")
  skip_for_target("r")
  gen <- odin({
    deriv(x) <- 1
    initial(x) <- 0
    print("x: {x}")
  }, debug_enable = TRUE)

  out <- capture_output(res <- gen$new()$run(0:5))
  expect_true(nzchar(out))
  out <- strsplit(out, "\n")[[1]]
  expect_match(out, "^\\[[0-9.]+\\] x: [0-9.]+$")
})


test_that_odin("don't print debugging if not enabled", {
  skip_for_target("js")
  skip_for_target("r")
  gen <- odin({
    deriv(x) <- 1
    initial(x) <- 0
    print("x: {x}")
  }, debug_enable = FALSE, skip_cache = TRUE)

  out <- capture_output(res <- gen$new()$run(0:5))
  expect_false(nzchar(out))
})


test_that_odin("print debug based on condition", {
  skip_for_target("js")
  skip_for_target("r")
  gen <- odin({
    deriv(x) <- 1
    initial(x) <- 0
    print("x: {x}", when = x > 1 && x < 4)
  }, debug_enable = TRUE)
  out <- capture_output(res <- gen$new()$run(0:5))
  expect_true(nzchar(out))
  out <- strsplit(out, "\n")[[1]]
  x <- as.numeric(sub(".+x: ", "", out))
  expect_true(all(x > 1 & x < 4))
})


test_that_odin("format to different levels of precision", {
  skip_for_target("js")
  skip_for_target("r")
  gen <- odin({
    deriv(x) <- 1
    initial(x) <- 0
    print("{x; .2f} {x; .4f} {x; .6f}")
  }, debug_enable = TRUE)
  out <- capture_output(res <- gen$new()$run(0:5))
  expect_true(nzchar(out))
  out <- strsplit(out, "\n")[[1]]
  expect_match(
    out,
    "^\\[[0-9]\\.[0-9]+\\] [0-9]\\.[0-9]{2} [0-9]\\.[0-9]{4} [0-9]\\.[0-9]{6}")
})
