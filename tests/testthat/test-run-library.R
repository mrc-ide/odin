context("run: library support")

test_that_odin("abs", {
  gen <- odin({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- abs(t)
  })
  tt <- seq(-5, 5, length.out = 101)
  expect_equal(gen$new()$run(tt)[, "a"], abs(tt))
})


test_that_odin("log", {
  gen <- odin({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- log(t)
    output(b) <- log(t, 2)
    output(c) <- log(t, 10)
  })
  tt <- seq(0.0001, 5, length.out = 101)
  yy <- gen$new()$run(tt)
  expect_equal(yy[, "a"], log(tt))
  expect_equal(yy[, "b"], log2(tt))
  expect_equal(yy[, "c"], log10(tt))
})


test_that_odin("pow", {
  gen <- odin({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- min(t, t^2 - 2, -t)
    output(b) <- max(t, t^2 - 2, -t)
  })
  tt <- seq(0.0001, 5, length.out = 101)
  yy <- gen$new()$run(tt)
  expect_equal(yy[, "a"], pmin(tt, tt^2 - 2, -tt))
  expect_equal(yy[, "b"], pmax(tt, tt^2 - 2, -tt))
})


test_that_odin("%%", {
  gen <- odin({
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
  mod <- gen$new()
  res <- mod$run(tt)
  s <- sin(1)
  q <- 1.0

  expect_equal(res[, "s1"],  tt %%  s)
  expect_equal(res[, "s2"], -tt %%  s)
  expect_equal(res[, "s3"],  tt %% -s)
  expect_equal(res[, "s4"], -tt %% -s)

  expect_equal(res[, "q1"],  tt %%  q)
  expect_equal(res[, "q2"], -tt %%  q)
  expect_equal(res[, "q3"],  tt %% -q)
  expect_equal(res[, "q4"], -tt %% -q)
})


test_that_odin("%/%", {
  ## As for %% but with %/%
  gen <- odin({
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
  mod <- gen$new()
  res <- mod$run(tt)
  s <- sin(1)
  q <- 1.0

  expect_equal(res[, "s1"],  tt %/%  s)
  expect_equal(res[, "s2"], -tt %/%  s)
  expect_equal(res[, "s3"],  tt %/% -s)
  expect_equal(res[, "s4"], -tt %/% -s)

  expect_equal(res[, "q1"],  tt %/%  q)
  expect_equal(res[, "q2"], -tt %/%  q)
  expect_equal(res[, "q3"],  tt %/% -q)
  expect_equal(res[, "q4"], -tt %/% -q)
})


test_that_odin("2-arg round", {
  gen <- odin({
    deriv(x) <- 1
    initial(x) <- 1
    output(y) <- TRUE
    output(z) <- TRUE
    n <- user(0)
    y <- round(t, n)
    z <- round(t)
  })

  mod0 <- gen$new(n = 0)
  mod1 <- gen$new(n = 1)
  mod2 <- gen$new(n = 2)

  tt <- seq(0, 1, length.out = 101)
  yy0 <- mod0$run(tt)
  yy1 <- mod1$run(tt)
  yy2 <- mod2$run(tt)

  expect_equal(yy0[, "z"], round(tt))
  expect_equal(yy1[, "z"], round(tt))
  expect_equal(yy2[, "z"], round(tt))

  expect_equal(yy0[, "y"], round(tt, 0))
  expect_equal(yy1[, "y"], round(tt, 1))
  expect_equal(yy2[, "y"], round(tt, 2))
})


test_that_odin("multivariate hypergeometric", {
  gen <- odin({
    x0[] <- user()
    dim(x0) <- user()
    n <- user()

    nk <- length(x0)

    ## We can't accept output from rmhyper (or e.g., rmultinom)
    ## directly into the state vector because the pointer types are
    ## incompatible.
    tmp[] <- rmhyper(n, x0)
    dim(tmp) <- nk
    output(tmp) <- TRUE

    initial(x[]) <- 0
    update(x[]) <- tmp[i]
    dim(x) <- nk
  })

  k <- c(6, 10, 15, 3, 0, 4)
  n <- 20
  mod <- gen$new(x0 = k, n = n)

  set.seed(1)
  res <- mod$run(0:10)
  set.seed(1)
  cmp <- t(replicate(10, rmhyper(n, k)))

  yy <- mod$transform_variables(res)
  expect_equal(yy$x[-1L, ], cmp)
  expect_equal(yy$tmp[-11L, ], yy$x[-1L, ])
})

test_that_odin("multivariate hypergeometric - integer input", {
  gen <- odin({
    x0[] <- user()
    dim(x0) <- user(integer = TRUE)
    n <- user(integer = TRUE)

    nk <- length(x0)

    ## We can't accept output from rmhyper (or e.g., rmultinom)
    ## directly into the state vector because the pointer types are
    ## incompatible.
    tot <- sum(x0)
    tmp[] <- rmhyper(tot, x0)
    tmp2[] <- rmhyper(n, tmp)

    initial(x[]) <- 0
    update(x[]) <- tmp[i]

    initial(y[]) <- 0
    update(y[]) <- tmp2[i]

    dim(tmp) <- nk
    dim(tmp2) <- nk
    dim(x) <- nk
    dim(y) <- nk
  })

  k <- c(6, 10, 15, 3, 0, 4)
  n <- 20
  mod <- gen$new(x0 = k, n = n)

  set.seed(1)
  res <- mod$run(0:10)
  set.seed(1)
  cmp <- t(replicate(10, rmhyper(n, k)))

  yy <- mod$transform_variables(res)
  expect_equal(yy$x[-1L, ], matrix(k, 10, 6, TRUE))
  expect_equal(yy$y[-1L, ], cmp)
})

test_that_odin("Throw an error if requesting more elements than possible", {
  gen <- odin({
    b[] <- user()
    n <- user()

    initial(x[]) <- 0
    update(x[]) <- x[i] + b[i]
    y[] <- rmhyper(n, x)
    output(y) <- TRUE

    dim(x) <- 3
    dim(b) <- 3
    dim(y) <- 3
  })
  b <- c(10, 15, 9)
  n <- 10
  mod <- gen$new(b = b, n = n)
  expect_error(mod$run(step = 2),
               "Requesting too many elements in rmhyper (10 from 0)",
               fixed = TRUE)
})


test_that_odin("Can use as.numeric", {
  gen <- odin({
    a <- user(integer = TRUE)
    b <- as.numeric(a)
    initial(x) <- 0
    update(x) <- x + b
  })
  mod <- gen$new(a = 5L)
  y <- mod$run(0:10)
  expect_equal(y[, "x"], seq(0, 50, by = 5))
})
