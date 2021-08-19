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


test_that("2d partial sum indexing correct", {
  d <- c(7, 13)
  m <- array(runif(prod(d)), d)

  expected <- list(
    "10" = rowSums(m),  # sum(m[i, ])
    "01" = colSums(m))  # sum(m[, i])

  info <- list(rank = 2, dimnames = list(dim = d))
  i <- seq_along(m) - 1L

  idx <- lapply(names(expected), function(key)
    eval(array_sum_lhs_index(key, info)) + 1L)

  ## What we need to do here is loop over the rhs and collect our answer:
  f <- function(idx, m) {
    res <- numeric(max(idx))
    for (i in seq_along(m)) {
      j <- idx[[i]]
      res[[j]] <- res[[j]] + m[[i]]
    }
    res
  }

  res <- set_names(lapply(idx, f, m), names(expected))
  expect_equal(res, expected)
})


test_that("3d partial sum indexing correct", {
  d <- c(5, 7, 13)
  m <- array(runif(prod(d)), d)

  expected <- list(
    "100" = apply(m, 1, sum),          # sum(x[i, , ])
    "010" = apply(m, 2, sum),          # sum(x[, i, ])
    "001" = apply(m, 3, sum),          # sum(x[, , i])
    "120" = c(apply(m, 1:2, sum)),     # sum(a[i, j, ])
    "102" = c(apply(m, c(1, 3), sum)), # sum(a[i, , j])
    "012" = c(apply(m, 2:3, sum)))     # sum(a[, i, j])

  info <- list(rank = 3, dimnames = list(dim = d, mult = cumprod(c(1, d))))
  i <- seq_along(m) - 1L

  idx <- lapply(names(expected), function(key)
    eval(array_sum_lhs_index(key, info)) + 1L)

  ## What we need to do here is loop over the rhs and collect our answer:
  f <- function(idx, m) {
    res <- numeric(max(idx))
    for (i in seq_along(m)) {
      j <- idx[[i]]
      res[[j]] <- res[[j]] + m[[i]]
    }
    res
  }

  res <- set_names(lapply(idx, f, m), names(expected))
  expect_equal(res, expected)
})


test_that("4d partial sum indexing correct", {
  d <- c(3, 5, 7, 13)
  m <- array(runif(prod(d)), d)

  expected <- list(
    "1000" = apply(m, 1, sum),             # sum(x[i, , , ])
    "0100" = apply(m, 2, sum),             # sum(x[, i, , ])
    "0010" = apply(m, 3, sum),             # sum(x[, , i, ])
    "0001" = apply(m, 4, sum),             # sum(x[, , , i])
    "1200" = c(apply(m, 1:2, sum)),        # sum(a[i, j, , ])
    "1020" = c(apply(m, c(1, 3), sum)),    # sum(a[i, , j, ])
    "1002" = c(apply(m, c(1, 4), sum)),    # sum(a[i, , , j])
    "1002" = c(apply(m, c(1, 4), sum)),    # sum(a[i, , , j])
    "0120" = c(apply(m, c(2, 3), sum)),    # sum(a[, i, j, ])
    "0102" = c(apply(m, c(2, 4), sum)),    # sum(a[, i, , j])
    "0012" = c(apply(m, c(3, 4), sum)),    # sum(a[, , i, j])
    "1230" = c(apply(m, c(1, 2, 3), sum)), # sum(a[i, j, k, ])
    "1203" = c(apply(m, c(1, 2, 4), sum)), # sum(a[i, j, , k])
    "1023" = c(apply(m, c(1, 3, 4), sum)), # sum(a[i, , j, k])
    "0123" = c(apply(m, c(2, 3, 4), sum))) # sum(a[, i, j, k])
  ## 15 combinations...

  info <- list(rank = 4, dimnames = list(dim = d, mult = cumprod(c(1, d))))
  i <- seq_along(m) - 1L

  idx <- lapply(names(expected), function(key)
    eval(array_sum_lhs_index(key, info)) + 1L)

  ## What we need to do here is loop over the rhs and collect our answer:
  f <- function(idx, m) {
    res <- numeric(max(idx))
    for (i in seq_along(m)) {
      j <- idx[[i]]
      res[[j]] <- res[[j]] + m[[i]]
    }
    res
  }

  res <- set_names(lapply(idx, f, m), names(expected))
  expect_equal(res, expected)
})


test_that("factorise sums where possible", {
  array_data <- function(name, rank) {
    ## There's no nice way of doing this really:
    dim <- lapply(seq_len(rank), array_dim_name, name = name)
    mult <- lapply(seq_len(rank), function(i)
      if (i == 1) ""
      else array_dim_name(name, paste(seq_len(i - 1), collapse = "")))
    list(name = name,
         rank = rank,
         dimnames = list(dim = dim, mult = mult))
  }

  data <- list(elements = list(a = array_data("a", 3),
                               b = array_data("a", 3),
                               m = array_data("m", 2)))
  sum_a <- list(name = quote(a),
                index = quote((i %/% dim_a_1) * dim_a_1 + i %% dim_a_1))
  expect_equal(
    factor_sum(quote(odin_sum(a, i, i, 1, dim(a, 2), j, j) + x[i, j]), data),
    list(base = quote(x[i, j]),
         sum = list(sum_a)))
  expect_equal(
    factor_sum(quote(x[i, j] + odin_sum(a, i, i, 1, dim(a, 2), j, j)), data),
    list(base = quote(x[i, j]),
         sum = list(sum_a)))

  ## works for multiple sums added
  expect_equal(
    factor_sum(quote(odin_sum(a, i, i, 1, dim(a, 2), j, j) +
                     odin_sum(a, i, i, 1, dim(a, 2), j, j)), data),
    list(base = NULL,
         sum = list(sum_a, sum_a)))
  expect_equal(
    factor_sum(quote(odin_sum(a, i, i, 1, dim(a, 2), j, j) +
                     odin_sum(a, i, i, 1, dim(a, 2), j, j) +
                     odin_sum(a, i, i, 1, dim(a, 2), j, j)), data),
    list(base = NULL,
         sum = list(sum_a, sum_a, sum_a)))

  expect_equal(
    factor_sum(quote(x[i, j] +
                     odin_sum(a, i, i, 1, dim(a, 2), j, j) +
                     b +
                     odin_sum(a, i, i, 1, dim(a, 2), j, j) +
                     c[i] +
                     odin_sum(a, i, i, 1, dim(a, 2), j, j)), data),
    list(base = quote(x[i, j] + b + c[i]),
         sum = list(sum_a, sum_a, sum_a)))
})


test_that("Can optimise simple sums", {
  ## TODO: consider moving this test into run?
  skip_on_cran()
  code <- quote({
    deriv(y) <- 0
    initial(y) <- 1
    m[, ] <- user()
    dim(m) <- user()
    v1[] <- sum(m[i, ])
    dim(v1) <- dim(m, 1)
    v2[] <- sum(m[, i])
    dim(v2) <- dim(m, 2)
    output(v1) <- TRUE
    output(v2) <- TRUE
  })

  options1 <- odin_options(rewrite_sums = TRUE,
                           rewrite_constants = TRUE,
                           target = "c")
  options2 <- odin_options(target = "c")

  gen1 <- odin_(code, options = options1)
  gen2 <- odin_(code, options = options2)

  m <- matrix(runif(35), 5)
  mod1 <- gen1$new(m = m)
  mod2 <- gen2$new(m = m)

  y1 <- mod1$contents()
  y2 <- mod2$contents()
  expect_equal(y1$v1, y2$v1)
  expect_equal(y1$v2, y2$v2)
})


test_that("Can optimise sums with base components", {
  ## TODO: consider moving this test into run?
  skip_on_cran()
  code <- quote({
    deriv(y) <- 0
    initial(y) <- 1
    a[, , ] <- user()
    dim(a) <- user()
    x[, ] <- user()
    dim(x) <- c(dim(a, 1), dim(a, 3))
    m[, ] <- sum(a[i, , j]) + x[i, j]
    dim(m) <- c(dim(x, 1), dim(x, 2))
    output(m) <- TRUE
  })

  options1 <- odin_options(rewrite_sums = TRUE,
                           # rewrite_constants = TRUE,
                           target = "c")
  options2 <- odin_options(target = "c")

  gen1 <- odin_(code, options = options1)
  gen2 <- odin_(code, options = options2)

  set.seed(1)
  a <- array(runif(3 * 5 * 7), c(3, 5, 7))
  x <- matrix(runif(3 * 7), 3, 7)
  mod1 <- gen1$new(a = a, x = x)
  mod2 <- gen2$new(a = a, x = x)

  ## This is badly wrong, with the '1' version being clearly incorrect
  y1 <- mod1$contents()
  y2 <- mod2$contents()
  expect_equal(y1$m, y2$m)
  expect_equal(sum(y1$m), sum(a) + sum(x))
})
