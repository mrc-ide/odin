context("generate")

test_that("rewrite functions", {
  gen <- odin2({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- abs(t)
  })
  tt <- seq(-5, 5, length.out = 101)
  expect_equal(gen()$run(tt)[, "a"], abs(tt))

  gen <- odin2({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- log(t)
    output(b) <- log(t, 2)
    output(c) <- log(t, 10)
  })
  tt <- seq(0.0001, 5, length.out = 101)
  yy <- gen()$run(tt)
  expect_equal(yy[, "a"], log(tt))
  expect_equal(yy[, "b"], log2(tt))
  expect_equal(yy[, "c"], log10(tt))

  ## TODO: can be done with parse_expr now
  expect_error(odin2({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- log()
  }), "Expected 1-2 arguments in log call")
  expect_error(odin2({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- log(1, 2, 3)
  }), "Expected 1-2 arguments in log call")

  gen <- odin2({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- min(t, t^2 - 2, -t)
    output(b) <- max(t, t^2 - 2, -t)
  })
  yy <- gen()$run(tt)
  expect_equal(yy[, "a"], pmin(tt, tt^2 - 2, -tt))
  expect_equal(yy[, "b"], pmax(tt, tt^2 - 2, -tt))

  gen <- odin2({
    deriv(y) <- 0
    initial(y) <- 0
    s <- sin(1) # does not appear exactly
    q <- 1.0    # appears exactly
    output(s1) <-  t %%  s
    output(s2) <- -t %%  s
    output(s3) <-  t %% -s
    output(s4) <- -t %% -s
    output(q1) <-  t %%  q
    output(q2) <- -t %%  q
    output(q3) <-  t %% -q
    output(q4) <- -t %% -q
  })
  tt <- seq(-5, 5, length.out = 101)
  mod <- gen()
  res <- mod$run(tt)
  s <- mod$contents()[["s"]]
  q <- mod$contents()[["q"]]

  expect_equal(res[, "s1"],  tt %%  s)
  expect_equal(res[, "s2"], -tt %%  s)
  expect_equal(res[, "s3"],  tt %% -s)
  expect_equal(res[, "s4"], -tt %% -s)

  expect_equal(res[, "q1"],  tt %%  q)
  expect_equal(res[, "q2"], -tt %%  q)
  expect_equal(res[, "q3"],  tt %% -q)
  expect_equal(res[, "q4"], -tt %% -q)

  ## As above, but for %/% not %%
  gen <- odin2({
    deriv(y) <- 0
    initial(y) <- 0
    s <- sin(1) # does not appear exactly
    q <- 1.0    # appears exactly
    output(s1) <-  t %/%  s
    output(s2) <- -t %/%  s
    output(s3) <-  t %/% -s
    output(s4) <- -t %/% -s
    output(q1) <-  t %/%  q
    output(q2) <- -t %/%  q
    output(q3) <-  t %/% -q
    output(q4) <- -t %/% -q
  })
  tt <- seq(-5, 5, length.out = 101)
  mod <- gen()
  res <- mod$run(tt)
  s <- mod$contents()[["s"]]
  q <- mod$contents()[["q"]]

  expect_equal(res[, "s1"],  tt %/%  s)
  expect_equal(res[, "s2"], -tt %/%  s)
  expect_equal(res[, "s3"],  tt %/% -s)
  expect_equal(res[, "s4"], -tt %/% -s)

  expect_equal(res[, "q1"],  tt %/%  q)
  expect_equal(res[, "q2"], -tt %/%  q)
  expect_equal(res[, "q3"],  tt %/% -q)
  expect_equal(res[, "q4"], -tt %/% -q)
})

test_that("array index rewriting", {
  skip("not for us")
  expr <- quote(idx[i])
  rw <- function(x) rewrite_c(x, "p", "idx")
  expect_equal(rw(quote(i)), structure("i", is_index = TRUE))
  expect_equal(rw(quote(1)), structure("1", is_index = FALSE))
  expect_equal(rw(quote(i + 1)), structure("i + 1", is_index = TRUE))

  expect_equal(rw(quote(idx[i])), structure("p->idx[i]", is_index = FALSE))
  expect_equal(rw(quote(x[i])), structure("x[i]", is_index = FALSE))

  expect_equal(minus1(quote(idx[i]), rw), "p->idx[i] - 1")
  expect_equal(minus1(quote(x[i]), rw), "x[i] - 1")

  expect_equal(rw(quote(x[idx[i]])),
               structure("x[p->idx[i] - 1]", is_index = FALSE))

  expect_equal(minus1(quote(2 + -3 + 4 - 5), rw),
               "2 + -3 + 4 - 6")
})
