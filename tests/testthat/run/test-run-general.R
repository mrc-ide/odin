context("run: %TARGET%: general")

## TODO: these should be split up eventually

## Tests of the approach against some known models.
test_that("constant model", {
  gen <- odin({
    deriv(y) <- 0.5
    initial(y) <- 1
  })
  mod <- gen()
  expect_identical(r6_private(mod)$init, 1.0)
  expect_identical(mod$deriv(0.0, r6_private(mod)$init), 0.5)

  tt <- seq(0, 10, length.out = 11)
  yy <- mod$run(tt)
  expect_equal(yy[, 2L], seq(1.0, length.out = length(tt), by = 0.5))

  ## Can avoid having column names:
  expect_null(colnames(mod$run(tt, use_names = FALSE)))
})

test_that("user variables", {
  gen <- odin({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user(1)
    K <- user(100)
    r <- user()
  })

  ## Two different errors when r is not provided:
  expect_error(gen(), 'argument "r" is missing')
  expect_error(gen(NULL), "Expected a value for 'r'")

  mod <- gen(pi)
  dat <- mod$contents()
  expect_equal(dat$r, pi)
  expect_equal(dat$N0, 1.0)
  expect_equal(dat$K, 100.0)

  ## This should be a noop:
  mod$set_user()
  dat <- mod$contents()
  expect_equal(dat$r, pi)
  expect_equal(dat$N0, 1.0)
  expect_equal(dat$K, 100.0)

  ## Now, try setting one of these:
  mod$set_user(N0 = 5)
  dat <- mod$contents()
  expect_equal(dat$r, pi)
  expect_equal(dat$N0, 5.0)
  expect_equal(dat$K, 100.0)

  ## Don't reset to default on subsequent set:
  mod$set_user()
  expect_equal(mod$contents()$N0, 5.0)
})

test_that("user variables on models with none", {
  gen <- odin({
    a <- 1
    deriv(y) <- 0.5 * a
    initial(y) <- 1
  })
  expect_error(gen(a = 1), "unused argument")
  mod <- gen()
  ## NOTE: This is a change of behaviour, but that's probably OK
  expect_silent(mod$set_user())
  expect_warning(mod$set_user(a = 1), "Unknown user parameters: a")
})

test_that("non-numeric time", {
  ## Only an issue for delay models or models with time-dependent
  ## initial conditions.
  gen <- odin({
    ylag <- delay(y, 10)
    initial(y) <- 0.5
    deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
  })
  mod <- gen()
  t <- as.integer(0:10)
  expect_equal(mod$initial(t), 0.5)
  expect_silent(mod$run(t))
})

test_that("delays and initial conditions", {
  gen <- odin({
    ylag <- delay(y, 10)
    initial(y) <- 0.5
    deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
  })

  mod <- gen()
  t <- as.integer(0:10)
  res1 <- mod$run(t)

  dat <- mod$contents()
  expect_equal(dat$initial_t, 0.0)
  expect_equal(dat$initial_y, 0.5)

  res2 <- mod$run(t + 1)
  expect_equal(res2[, 2], res1[, 2])
  expect_equal(mod$contents()$initial_t, 1.0)

  ## Trickier; pass the initial conditions through and have them set
  ## into the model so delays work correctly.
  res3 <- mod$run(t + 2, 0.5)
  expect_equal(res3[, 2], res1[, 2], tolerance = 1e-7)
  expect_equal(mod$contents()$initial_t, 2.0)
  expect_equal(mod$contents()$initial_y, 0.5)

  res4 <- mod$run(t + 3, 0.6)

  expect_equal(mod$contents()$initial_t, 3.0)
  expect_equal(mod$contents()$initial_y, 0.6)
  expect_false(isTRUE(all.equal(res4[, 2], res1[, 2])))
})

test_that("non-numeric user", {
  gen <- odin({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user(1)
    K <- user(100)
    r <- user()
  })
  mod <- gen(1L)
  expect_is(mod$contents()$r, "numeric")
  expect_identical(mod$contents()$r, 1.0)
})

test_that("conditionals", {
  gen <- odin({
    deriv(x) <- if (x > 2) 0 else 0.5
    initial(x) <- 0
  })

  ## Hey ho it works:
  mod <- gen()
  t <- seq(0, 5, length.out = 101)
  y <- mod$run(t)

  expect_equal(y[, 2], ifelse(t < 4, t * 0.5, 2.0), tolerance = 1e-5)
})

test_that("conditionals, precendence", {
  gen <- odin({
    deriv(x) <- 0.1 + 2 * if (t > 2) -0.1 else 0.5
    initial(x) <- 0
  })

  mod <- gen()
  t <- seq(0, 5, length.out = 101)
  y <- mod$run(t)

  cmp <- ifelse(t < 2, 1.1 * t, 2.4 -0.1 * t)
  expect_equal(y[, 2], cmp, tolerance = 1e-5)
})

test_that("time dependent", {
  ## A time dependent initial condition:
  gen_t <- odin({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- sqrt(t) + 1
    K <- 100
    r <- 0.5
  })

  ## The same model, but taking N0 as a user parameter.
  gen_cmp <- odin({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user()
    K <- 100
    r <- 0.5
  })

  mod_t <- gen_t()
  expect_equal(mod_t$initial(0), 1)
  expect_equal(mod_t$initial(10), sqrt(10) + 1)

  t0 <- seq(0,  10, length.out = 101)
  t1 <- seq(10, 10, length.out = 101)

  expect_equal(mod_t$run(t0), gen_cmp(sqrt(t0[[1]]) + 1)$run(t0))
  expect_equal(mod_t$run(t1), gen_cmp(sqrt(t1[[1]]) + 1)$run(t1))
})

test_that("time dependent initial conditions", {
  gen <- odin({
    y1 <- sin(t)
    deriv(y2) <- y1
    initial(y2) <- -1
    output(y1) <- y1
  })

  mod <- gen()
  t <- seq(0, 2 * pi, length.out = 101)
  y <- mod$run(t, atol = 1e-8, rtol = 1e-8)
  expect_identical(y[, 3L], sin(t))
  expect_equal(y[, 2L], cos(t + pi), tolerance = 1e-6)
})

test_that("user c", {
  skip_for_target("r")
  gen <- odin({
    config(include) <- "user_fns.c"
    z <- squarepulse(t, 1, 2)
    output(z) <- z
    deriv(y) <- z
    initial(y) <- 0
  })

  mod <- gen()
  t <- seq(0, 3, length.out = 301)
  y <- mod$run(t)

  expect_equal(y[, 3L], as.numeric(t >= 1 & t < 2))
  cmp <- -1 + t
  cmp[t < 1] <- 0
  cmp[t > 2] <- 1
  expect_equal(y[, 2L], cmp, tolerance = 1e-5)
})

test_that("user c in subdir", {
  skip_for_target("r")
  dest <- tempfile()
  dir.create(dest)

  expr <- c('config(include) <- "myfuns.c"',
            "z <- squarepulse(t, 1, 2)",
            "output(z) <- z",
            "deriv(y) <- z",
            "initial(y) <- 0")
  test <- file.path(dest, "test.R")
  writeLines(expr, test)

  expect_error(odin_(test), "Could not find file 'myfuns.c'",
               class = "odin_error")

  file.copy("user_fns.c", file.path(dest, "myfuns.c"))
  gen <- odin_(test)

  ## copied from above:
  mod <- gen()
  t <- seq(0, 3, length.out = 301)
  y <- mod$run(t)

  expect_equal(y[, 3L], as.numeric(t >= 1 & t < 2))
  cmp <- -1 + t
  cmp[t < 1] <- 0
  cmp[t > 2] <- 1
  expect_equal(y[, 2L], cmp, tolerance = 1e-5)
})

test_that("time dependent initial conditions", {
  gen <- odin({
    y1 <- cos(t)
    y2 <- y1 * (1 + t)
    deriv(y3) <- y2
    initial(y3) <- y2
    output(y1) <- y1
    output(y2) <- y2
  })

  mod <- gen()

  ## Initial conditions get through here:
  expect_equivalent(mod$initial(0), 1)
  expect_equivalent(mod$initial(1), cos(1) * 2)

  t <- seq(0, 4 * pi, length.out = 101)
  y <- mod$run(t, atol = 1e-8, rtol = 1e-8)
  expect_equal(as.vector(y[1, 2]), 1.0)
  ## TODO: Compute analytic expectation and compare here.
  expect_equal(as.vector(y[length(t), 2]), 1.0, tolerance = 1e-7)
})

test_that("time dependent initial conditions depending on vars", {
  gen <- odin({
    v1 <- exp(-t)

    initial(y1) <- 1
    deriv(y1) <- y1 * v1

    deriv(y2) <- y2 * 0.5
    initial(y2) <- y1 + v1

    deriv(y3) <- y3 * 0.1
    initial(y3) <- y1 + y2
  })

  mod <- gen()
  expect_equal(mod$initial(0), c(1, 2, 3))
  expect_equal(mod$initial(1), c(1, 1 + exp(-1), 2 + exp(-1)))
})

## This test case kindly contributed by @blackedder in #14
test_that("unused variable in output", {
  gen <- odin({
    initial(S) <- N - I0
    initial(E1) <- 0
    initial(E2) <- 0
    initial(I1) <- I0
    initial(I2) <- 0
    initial(R) <- 0

    N <- 1e7
    I0 <- 1

    lambda <- 0.00001 * (I1 + I2)
    gamma1 <- 2.5
    gamma2 <- 1.1

    deriv(S) <- -lambda * S
    deriv(E1) <- lambda * S - gamma1 * E1
    deriv(E2) <- gamma1 * (E1 - E2)
    deriv(I1) <- gamma1 * E2  - gamma2 * I1
    deriv(I2) <- gamma2 * (I1 - I2)
    deriv(R) <- gamma2 * I2

    output(tot) <- S + E1 + E2 + I1 + I2 + R
  })
  mod <- gen()
  expect_is(mod, "odin_model")
  t <- seq(0, 10, length.out = 100)
  expect_error(mod$run(t), NA)
})

test_that("3d array", {
  gen <- odin({
    initial(y[,,]) <- 1
    deriv(y[,,]) <- y[i,j,k] * 0.1
    dim(y) <- c(2, 3, 4)
  })
  mod <- gen()
  expect_equal(mod$initial(), rep(1.0, 2 * 3 * 4))

  tt <- seq(0, 10, length.out = 11)
  yy <- mod$run(tt)

  ## We now have nicely named output:
  expect_match(colnames(yy)[-1], "^y\\[[0-9],[0-9],[0-9]\\]$")

  ## Transform for even nicer:
  zz <- mod$transform_variables(yy)
  expect_equal(dim(zz$y), c(c(length(tt), 2, 3, 4)))

  ## Check the automatic variable naming:
  expect_identical(zz$y[, 1, 2, 4], yy[, "y[1,2,4]"])

  ## Check conversion of single row:
  y0 <- mod$transform_variables(yy[1,])
  expect_equal(y0,
               c(setNames(list(tt[1]), TIME), list(y = array(1, c(2, 3, 4)))))
})

test_that("4d array", {
  ## TODO: offset_y is saved here and is not really needed.
  gen <- odin({
    initial(y[,,,]) <- 1
    deriv(y[,,,]) <- y[i,j,k,l] * 0.1
    dim(y) <- c(2, 3, 4, 5)
  })

  mod <- gen()
  expect_equal(mod$initial(), rep(1.0, 2 * 3 * 4 * 5))
  dat <- mod$contents()
  expect_equal(dat$initial_y, array(1, c(2, 3, 4, 5)))
})

## I need a system with mixed variables and arrays for testing the
## parse code.  This is going to be a really stupid system!
test_that("mixed", {
  gen <- odin({
    deriv(a) <- r * a
    initial(a) <- 1
    deriv(b) <- r * b
    initial(b) <- 1
    deriv(v[]) <- r * v[i]
    initial(v[]) <- 1
    dim(v) <- 3
    r <- 0.1
  })
  mod <- gen()
  expect_is(mod, "odin_model")
  t <- seq(0, 10, length.out = 100)
  y <- mod$run(t)
  expect_error(y, NA) # just test that it doesn't fail

  yy <- mod$transform_variables(y)
  expect_equal(sort(names(yy)), sort(c(TIME, "a", "b", "v")))

  ## Check contents:
  expect_equal(yy[c(TIME, "a", "b")],
               as.list(as.data.frame(y[, c(TIME, "a", "b")])))
  expect_equal(yy$v, unname(y[, sprintf("v[%d]", 1:3)]))

  ## Check scalar:
  y0 <- mod$transform_variables(y[1, ])
  expect_equal(names(y0), names(yy))
  expect_equal(y0,
               lapply(yy, function(x) if (is.matrix(x)) x[1, ] else x[[1]]))
})


## TODO: We're ambiguous with output dim.
##
## This would probably work but be bad:
##
##   output(y[]) <- y[i] * 2
##   dim(y) <- 10
##
## because we'd pick up dim(output(y)) as 10; most of the time this
## would be correct but sometimes might not be.  The check is:
##
## disallow *array* output that is nontrivial that shares a name with
## any other variable.

## Output array
##
## (1) A new array:
test_that("output array", {
  gen <- odin({
    deriv(y[]) <- r[i] * y[i]
    initial(y[]) <- 1
    r[] <- 0.1
    dim(r) <- 3
    dim(y) <- 3
    ## testing below here:
    output(y2[]) <- y[i] * 2
    ## NOTE: Not dim(output(y2)) [TODO: should we support this?]
    dim(y2) <- 3 # length(y) -- TODO -- should be OK?
  })

  mod <- gen()
  tt <- seq(0, 10, length.out = 101)
  yy <- mod$run(tt)

  expect_equal(colnames(yy), c("t",
                               sprintf("y[%d]", 1:3),
                               sprintf("y2[%d]", 1:3)))

  ## transform function:
  zz <- mod$transform_variables(yy)
  expect_equal(zz$y2, zz$y * 2)
})

## (2) An existing array
test_that("output array", {
  gen <- odin({
    deriv(y[]) <- r[i] * y[i]
    initial(y[]) <- 1
    r[] <- 0.1
    dim(r) <- 3
    dim(y) <- 3
    ## This should probably be OK, but might need some more trickery...
    output(r[]) <- r
  })

  mod <- gen()
  tt <- seq(0, 10, length.out = 101)
  yy <- mod$run(tt)

  expect_equal(colnames(yy), c("t",
                               sprintf("y[%d]", 1:3),
                               sprintf("r[%d]", 1:3)))

  ## transform function:
  zz <- mod$transform_variables(yy)
  expect_equal(zz$r, matrix(0.1, length(tt), 3))
})


test_that("use length on rhs", {
  gen <- odin({
    deriv(y[]) <- r[i] * y[i]
    initial(y[]) <- 1
    r[] <- 0.1
    dim(y) <- 3
    dim(r) <- length(y)
  })

  mod <- gen()
  expect_equal(mod$contents()$r, rep(0.1, 3))
})

test_that("use dim on rhs", {
  gen <- odin({
    deriv(y[,]) <- r[i] * y[i,j]
    initial(y[,]) <- 1
    r[] <- 0.1
    dim(y) <- c(3, 4)
    dim(r) <- dim(y, 1)
  })

  mod <- gen()
  expect_equal(mod$contents()$r, rep(0.1, 3))
  expect_equal(mod$contents()$initial_y, matrix(1, 3, 4))
})


## Ideally we'll end up with all combinations of has array/has scalar
## (there are 15 possible combinations though!)
test_that("transform variables with output", {
  gen <- odin({
    deriv(y[]) <- r[i] * y[i]
    initial(y[]) <- y0[i]
    r[] <- user()
    dim(r) <- user()
    dim(y) <- length(r)
    y0[] <- user()
    dim(y0) <- length(r)
    output(a) <- sum(y)
  })

  y0 <- runif(3)
  r <- runif(3)
  mod <- gen(y0 = y0, r = r)

  tt <- seq(0, 5, length.out = 101)
  real_y <- t(y0 * exp(outer(r, tt)))
  real_a <- rowSums(real_y)

  y <- mod$run(tt, atol = 1e-8, rtol = 1e-8)
  yy <- mod$transform_variables(y)

  expect_equal(yy$y, real_y)
  expect_equal(yy$a, real_a)
})


test_that("transform variables without time", {
  gen <- odin({
    deriv(y[]) <- r[i] * y[i]
    initial(y[]) <- y0[i]
    r[] <- user()
    dim(r) <- user()
    dim(y) <- length(r)
    y0[] <- user()
    dim(y0) <- length(r)
    output(a) <- sum(y)
  })

  y0 <- runif(3)
  r <- runif(3)
  mod <- gen(y0 = y0, r = r)

  tt <- seq(0, 5, length.out = 101)
  yy <- mod$run(tt, atol = 1e-8, rtol = 1e-8)

  cmp <- mod$transform_variables(yy)
  res <- mod$transform_variables(yy[, -1])
  expect_equal(names(res), names(cmp))
  expect_equal(res$t, rep(NA_real_, length(tt)))
  expect_equal(res[names(res) != "t"], cmp[names(cmp) != "t"])

  cmp <- mod$transform_variables(yy[1, ])
  res <- mod$transform_variables(yy[1, -1])
  expect_equal(names(res), names(cmp))
  expect_equal(res$t, NA_real_)
  expect_equal(res[names(res) != "t"], cmp[names(cmp) != "t"])

  expect_error(mod$transform_variables(yy[, -(1:2)]),
               "Unexpected size input")
  expect_error(mod$transform_variables(cbind(yy, yy)),
               "Unexpected size input")
})


test_that("pathalogical array index", {
  gen <- odin({
    deriv(z) <- y1 + y2 + y3 + y4 + y5
    initial(z) <- 0

    ## This one is a bit of a worry, frankly - everything is off by
    ## one.  It looks to me that the issue here is that in the
    ## *initial assignment* we have assigned the wrong thing.  I think
    ## that Ada has an issue about this actually!  Probably this will
    ## require some care on the rewrite.
    y[] <- i #  + 1
    dim(y) <- 5

    a <- length(y)

    y1 <- y[a + 1 - a] # y[1] -- first call is '-'
    y2 <- y[2 - a + a] # y[2] -- first call is '+'
    y3 <- y[1 + 2] # y[3]
    y4 <- y[a - 1] # y[4]
    y5 <- y[5 + (a - a)] # y[5]
  })

  dat <- gen()$contents()
  expect_equal(dat$y1, 1.0)
  expect_equal(dat$y2, 2.0)
  expect_equal(dat$y3, 3.0)
  expect_equal(dat$y4, 4.0)
  expect_equal(dat$y5, 5.0)
})


test_that("two output arrays", {
  gen <- odin({
    deriv(y[]) <- y[i] * r[i]
    initial(y[]) <- i
    dim(y) <- 3
    dim(r) <- 3
    r[] <- user()
    output(yr[]) <- y[i] / i
    dim(yr) <- 3
    output(r[]) <- TRUE
  })

  r <- runif(3)
  mod <- gen(r = r)
  tt <- seq(0, 10, length.out = 101)
  yy <- mod$run(tt, atol = 1e-8, rtol = 1e-8)
  zz <- mod$transform_variables(yy)

  expect_equal(zz$y, t(1:3 * exp(outer(r, tt))), tolerance = 1e-6)
  expect_equal(zz$r, matrix(r, length(tt), 3, TRUE))
  expect_equal(zz$yr, t(t(zz$y) / (1:3)))

  ## An extension of the above that tickles an array size problem
  gen2 <- odin({
    deriv(y[]) <- y[i] * r[i]
    initial(y[]) <- y0[i]
    dim(y) <- length(y0)
    dim(r) <- length(y0)
    y0[] <- user()
    r[] <- user()
    dim(y0) <- user()
    output(yr[]) <- y[i] / y0[i]
    dim(yr) <- length(y0)
    output(r[]) <- TRUE
  })

  mod2 <- gen2(y0 = as.numeric(1:3), r = r)
  res <- mod2$run(tt, atol = 1e-8, rtol = 1e-8)
  expect_equal(res, yy)
})

## TODO: This still needs harmonising with get_user_array1 functions
## (non user dimensions) as they use coerceVector still.
test_that("non-numeric input", {
  gen <- odin({
    deriv(y) <- 1
    initial(y) <- 1
    scalar <- user()
    vector[] <- user()
    dim(vector) <- user()
    matrix[,] <- user()
    dim(matrix) <- user()
    array[,,] <- user()
    dim(array) <- user()
    array4[,,,] <- user()
    dim(array4) <- user()
  })

  scalar <- 1
  vector <- as.numeric(1:3)
  matrix <- matrix(as.numeric(1:prod(2:3)), 2L, 3L)
  array <- array(as.numeric(1:prod(2:4)), c(2L, 3L, 4L))
  array4 <- array(as.numeric(1:prod(2:5)), c(2L, 3L, 4L, 5L))

  convert <- function(x, to = "integer") {
    storage.mode(x) <- to
    if (to == "character") {
      x[] <- paste(x, "number")
    }
    x
  }

  ## First, this is all easy and has been well tested already:
  mod <- gen(scalar = scalar,
             vector = vector,
             matrix = matrix,
             array = array,
             array4 = array4)
  dat <- mod$contents()

  expect_equal(dat$scalar, scalar)
  expect_equal(dat$vector, vector)
  expect_equal(dat$matrix, matrix)
  expect_equal(dat$array,  array)
  expect_equal(dat$array4, array4)

  ## Then to integer first:
  mod <- gen(scalar = convert(scalar),
             vector = convert(vector),
             matrix = convert(matrix),
             array = convert(array),
             array4 = convert(array4))
  dat <- mod$contents()
  expect_equal(dat$scalar, scalar)
  expect_equal(dat$vector, vector)
  expect_equal(dat$matrix, matrix)
  expect_equal(dat$array,  array)
  expect_equal(dat$array4, array4)

  ## Then test for errors on each as we convert to character:
  expect_error(
    gen(scalar = convert(scalar, "character"),
        vector = vector,
        matrix = matrix,
        array = array,
        array4 = array4),
    "Expected a numeric value for scalar")
  expect_error(
    gen(scalar = scalar,
        vector = convert(vector, "character"),
        matrix = matrix,
        array = array,
        array4 = array4),
    "Expected a numeric value for vector")
  expect_error(
    gen(scalar = scalar,
        vector = vector,
        matrix = convert(matrix, "character"),
        array = array,
        array4 = array4),
    "Expected a numeric value for matrix")
  expect_error(
    gen(scalar = scalar,
        vector = vector,
        matrix = matrix,
        array = convert(array, "character"),
        array4 = array4),
    "Expected a numeric value for array")
  expect_error(
    gen(scalar = scalar,
        vector = vector,
        matrix = matrix,
        array = array,
        array4 = convert(array4, "character")),
    "Expected a numeric value for array4")
})

test_that("only used in output", {
  gen <- odin({
    deriv(y[]) <- r[i] * y[i]
    initial(y[]) <- 1
    r[] <- 0.1
    dim(r) <- 3
    dim(y) <- 3
    ## output only:
    tot <- sum(y)
    output(ytot) <- tot
    output(y2[]) <- y[i] * 2
    dim(y2) <- length(y)
  })

  mod <- gen()
  tt <- seq(0, 10, length.out = 101)
  res <- mod$transform_variables(mod$run(tt))
  expect_equal(res$ytot, rowSums(res$y))
  expect_equal(res$y2, res$y * 2)
})

test_that("overlapping graph", {
  gen <- odin({
    deriv(y) <- y * p
    initial(y) <- 1
    r <- -0.5
    p <- r * sqrt(t) # used in both deriv and output
    p2 <- p * 2 # used in output only
    output(p3) <- p + p2
  }, verbose = FALSE)

  mod <- gen()
  tt <- seq(0, 10, length.out = 101)

  f <- function(t, y, p) {
    r <- -0.5
    p <- r * sqrt(t)
    p2 <- p * 2
    list(y * p, p + p2)
  }
  cmp <- deSolve::ode(1, tt, f, NULL)
  expect_equivalent(mod$run(tt), cmp)
})

test_that("sum over one dimension", {
  ## This does rowSums / colSums and will be important for building up
  ## towards a general sum.
  gen <- odin({
    deriv(y) <- 0
    initial(y) <- 1

    m[,] <- user()
    dim(m) <- user()

    v1[] <- sum(m[i, ])
    dim(v1) <- dim(m, 1)
    v2[] <- sum(m[, i])
    dim(v2) <- dim(m, 2)

    v3[] <- sum(m[i, 2:4])
    dim(v3) <- length(v1)
    v4[] <- sum(m[2:4, i])
    dim(v4) <- length(v2)

    tot1 <- sum(m)
    tot2 <- sum(m[,])
  }, verbose = FALSE)

  nr <- 5
  nc <- 7
  m <- matrix(runif(nr * nc), nr, nc)
  dat <- gen(m = m)$contents()

  expect_equal(dat$m, m)
  expect_equal(dat$v1, rowSums(m))
  expect_equal(dat$v2, colSums(m))

  expect_equal(dat$v3, rowSums(m[, 2:4]))
  expect_equal(dat$v4, colSums(m[2:4, ]))

  expect_equal(dat$tot1, sum(m))
  expect_equal(dat$tot2, sum(m))
})

test_that("sum over two dimensions", {
  ## This is where things get a bit more horrid:
  gen <- odin({
    deriv(y) <- 0
    initial(y) <- 1

    a[,,] <- user()
    dim(a) <- user()

    ## These collapse one dimension
    m12[,] <- sum(a[i, j, ])
    m13[,] <- sum(a[i, , j])
    m23[,] <- sum(a[, i, j])

    dim(m12) <- c(dim(a, 1), dim(a, 2))
    dim(m13) <- c(dim(a, 1), dim(a, 3))
    dim(m23) <- c(dim(a, 2), dim(a, 3))

    ## These collapse two dimensions
    v1[] <- sum(a[i, , ])
    v2[] <- sum(a[, i, ])
    v3[] <- sum(a[, , i])
    dim(v1) <- dim(a, 1)
    dim(v2) <- dim(a, 2)
    dim(v3) <- dim(a, 3)

    mm12[,] <- sum(a[i, j, 2:4])
    mm13[,] <- sum(a[i, 2:4, j])
    mm23[,] <- sum(a[2:4, i, j])
    ## TODO: dim(mm12) <- dim(m12) will not work, but that would be nice
    dim(mm12) <- c(dim(a, 1), dim(a, 2))
    dim(mm13) <- c(dim(a, 1), dim(a, 3))
    dim(mm23) <- c(dim(a, 2), dim(a, 3))

    vv1[] <- sum(a[i, 2:4, 2:4])
    vv2[] <- sum(a[2:4, i, 2:4])
    vv3[] <- sum(a[2:4, 2:4, i])
    dim(vv1) <- dim(a, 1)
    dim(vv2) <- dim(a, 2)
    dim(vv3) <- dim(a, 3)

    tot1 <- sum(a)
    tot2 <- sum(a[,,])
  }, verbose = FALSE)

  nr <- 5
  nc <- 7
  nz <- 9
  a <- array(runif(nr * nc * nz), c(nr, nc, nz))
  dat <- gen(a = a)$contents()

  expect_equal(dat$a, a)
  expect_equal(dat$m12, apply(a, 1:2, sum))
  expect_equal(dat$m13, apply(a, c(1, 3), sum))
  expect_equal(dat$m23, apply(a, 2:3, sum))

  expect_equal(dat$v1, apply(a, 1, sum))
  expect_equal(dat$v2, apply(a, 2, sum))
  expect_equal(dat$v3, apply(a, 3, sum))

  expect_equal(dat$mm12, apply(a[,,2:4], 1:2, sum))
  expect_equal(dat$mm13, apply(a[,2:4,], c(1, 3), sum))
  expect_equal(dat$mm23, apply(a[2:4,,], 2:3, sum))

  expect_equal(dat$vv1, apply(a[,2:4,2:4], 1, sum))
  expect_equal(dat$vv2, apply(a[2:4,,2:4], 2, sum))
  expect_equal(dat$vv3, apply(a[2:4,2:4,], 3, sum))

  expect_equal(dat$tot1, sum(a))
  expect_equal(dat$tot2, sum(a))
})

test_that("sum for a 4d array", {
  ## I don't want to check absolutely everything here, so hopefully if
  ## these few go OK then given the more exhaustive tests above we'll
  ## be OK
  gen <- odin({
    deriv(y) <- 0
    initial(y) <- 1

    a[,,,] <- user()
    dim(a) <- user()

    m12[,] <- sum(a[i, j, , ])
    m23[,] <- sum(a[, i, j, ])
    m24[,] <- sum(a[, i, , j])

    dim(m12) <- c(dim(a, 1), dim(a, 2))
    dim(m23) <- c(dim(a, 2), dim(a, 3))
    dim(m24) <- c(dim(a, 2), dim(a, 4))

    tot1 <- sum(a)
    tot2 <- sum(a[,,,])
  }, verbose = FALSE)

  dim <- c(3, 5, 7, 9)
  a <- array(runif(prod(dim)), dim)
  dat <- gen(a = a)$contents()

  expect_equal(dat$a, a)
  expect_equal(dat$m12, apply(a, 1:2, sum))
  expect_equal(dat$m23, apply(a, c(2, 3), sum))
  expect_equal(dat$m24, apply(a, c(2, 4), sum))
})

test_that("self output for scalar", {
  gen <- odin({
    initial(a) <- 1
    deriv(a) <- 0
    x <- t
    output(x) <- TRUE
  })

  tt <- seq(0, 10, length.out = 11)
  expect_equal(gen()$run(tt)[, "x"], tt)
})

test_that("non-time sentsitive output", {
  gen <- odin({
    initial(a) <- 1
    deriv(a) <- 0
    x <- 1
    output(x) <- TRUE
  })

  tt <- seq(0, 10, length.out = 11)
  expect_equal(gen()$run(tt)[, "x"], rep(1, length(tt)))
})

test_that("logical operations", {
  gen <- odin({
    initial(a) <- 1
    deriv(a) <- 0

    ## These ones are easy
    output(x1) <- t > 1 && t < 3
    output(x2) <- t > 1 || t < 3

    ## These ones may differ; note that parens are suggested by the
    ## compiler for this line.
    output(x3) <- t > 8 || t > 1 && t < 3 # should equal x4
    output(x4) <- t > 8 || (t > 1 && t < 3)
    output(x5) <- (t > 8 || t > 1) && t < 3
  }, compiler_warnings = FALSE)

  t <- seq(0, 10, length.out = 101)
  y <- gen()$run(t)

  expect_equal(y[, "x1"], as.numeric(t > 1 & t < 3))
  expect_equal(y[, "x2"], as.numeric(t > 1 | t < 3))
  expect_equal(y[, "x3"], as.numeric(t > 8 | t > 1 & t < 3))
  expect_equal(y[, "x4"], as.numeric(t > 8 | (t > 1 & t < 3)))
  expect_equal(y[, "x5"], as.numeric((t > 8 | t > 1) & t < 3))
})

## This is for issue #44, needed to support Neil's model.  I don't
## know how useful this is going to be.  I'll see if we can get away
## with this for now, and then go through and see if we can detect if
## a number is an integer thing because it's only used within indexes.
test_that("integer vector", {
  ## We expect 'idx' to come through as an integer
  gen <- odin({
    x[] <- user()
    dim(x) <- user()
    idx[] <- user()
    dim(idx) <- user()
    initial(v[]) <- x[idx[i]] # TODO: fixme
    deriv(v[]) <- 0
    dim(v) <- length(x)
  })

  set.seed(1)
  idx <- sample(15)
  x <- runif(length(idx))
  mod <- gen(x = x, idx = idx)
  dat <- mod$contents()
  expect_equal(dat$idx, idx)
  expect_equal(dat$initial_v, x[idx])

  expect_equal(ir_deserialise(mod$ir)$data$elements$idx$storage_type,
               "int")
})

## This is much closer to the test case needed for Neil's model
test_that("integer matrix", {
  gen <- odin({
    x[] <- user()
    dim(x) <- user()

    idx[, ] <- user()
    dim(idx) <- c(length(x), 3)

    v[] <- x[idx[i, 1]] + x[idx[i, 2]] + x[idx[i, 3]]
    dim(v) <- length(x)

    initial(z) <- 1
    deriv(z) <- 0
  })

  x <- runif(10)
  idx <- matrix(sample(length(x), length(x) * 3, replace = TRUE), length(x), 3)
  ## This is what the code should expand to:
  v <- x[idx[, 1]] + x[idx[, 2]] + x[idx[, 3]]

  mod <- gen(x = x, idx = idx)
  expect_equal(mod$contents()$v, v)
  expect_equal(ir_deserialise(mod$ir)$data$elements$idx$storage_type,
               "int")
})

test_that("c in dim for vector", {
  ## This is a regression test for issue #61
  gen <- odin({
    initial(x[]) <- 1
    deriv(x[]) <- 0
    dim(x) <- c(5)
  })
  mod <- gen()
  expect_equal(mod$contents()$initial_x, rep(1.0, 5))
})


test_that("user variable information", {
  gen <- odin({
    deriv(N) <- r[1] * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user(1)
    K <- user(100)
    r[] <- user()
    dim(r) <- 1
  })

  info <- coef(gen)
  expect_is(info, "data.frame")
  expect_equal(info$name, names(formals(gen))[1:3])
  expect_equal(info$has_default, c(FALSE, TRUE, TRUE))
  expect_equal(info$rank, c(1L, 0L, 0L))

  expect_identical(coef(gen(1)), info)
})


test_that("user variable information - when no user", {
  gen <- odin({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- 10
    K <- 100
    r <- 0.1
  })
  info <- coef(gen)
  cmp <- data.frame(name = character(),
                    has_default = logical(),
                    default_value = I(list()),
                    rank = integer(),
                    min = numeric(),
                    max = numeric(),
                    integer = logical(),
                    stringsAsFactors = FALSE)
  expect_identical(info, cmp)
  expect_identical(coef(gen()), cmp)
})


test_that("format/print", {
  gen <- odin({
    deriv(N) <- r[1] * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user(1)
    K <- user(100)
    r[] <- user()
    dim(r) <- 1
  })

  txt <- capture.output(x <- withVisible(print(gen)))
  expect_match(txt,
               capture.output(args(gen))[[1]],
               fixed = TRUE, all = FALSE)
  expect_match(txt,
               "<an 'odin_generator' function>",
               fixed = TRUE, all = FALSE)
  expect_match(txt,
               "use coef() to get information on user parameters",
               fixed = TRUE, all = FALSE)

  expect_identical(x, list(value = gen, visible = FALSE))
})


test_that("ir is read-only", {
  gen <- odin({
    deriv(y) <- 0.5
    initial(y) <- 1
  })
  mod <- gen()
  ir <- mod$ir
  expect_error(mod$ir <- TRUE)
  expect_identical(mod$ir, ir)
})


test_that("multiline string", {
  ## Literal multiline string:
  gen <- odin(c("deriv(y) <- 0.5", "initial(y) <- 1"))
  expect_is(gen(), "odin_model")
})


## This is basically all ok but what is still not great is _doing_ the
## validation.
test_that("user integer", {
  gen <- odin({
    deriv(y) <- 0.5
    initial(y) <- y0
    y0 <- user(1, integer = TRUE, min = 0)
  })

  expect_error(gen(y0 = 1.5), "Expected 'y0' to be integer-like")
  expect_error(gen(y0 = -1L), "Expected 'y0' to be at least 0")

  expect_error(mod <- gen(y0 = 1), NA)
  expect_equal(mod$run(0:10)[, "y"], 1.0 + 0.5 * (0:10))
})


test_that("multiple constraints", {
  gen <- odin({
    deriv(y) <- r
    initial(y) <- y0
    y0 <- user(1, min = 0)
    r <- user(0.5, max = 10)
  })

  expect_error(gen(y0 = -1L), "Expected 'y0' to be at least 0")
  expect_error(gen(r = 100), "Expected 'r' to be at most 10")
})


test_that("set_user honours constraints", {
  gen <- odin({
    deriv(y) <- r
    initial(y) <- y0
    y0 <- user(1, min = 0)
    r <- user(0.5, max = 10)
  })

  mod <- gen()
  expect_error(mod$set_user(y0 = -1L), "Expected 'y0' to be at least 0")
  expect_error(mod$set_user(r = 100), "Expected 'r' to be at most 10")
})


test_that("user sized dependent variables are allowed", {
  gen <- odin({
    deriv(y[]) <- r[i] * y[i]
    initial(y[]) <- 1
    r[] <- user()
    dim(r) <- user()
    dim(y) <- length(r)
  })
  r <- runif(3)
  mod <- gen(r = r)
  expect_identical(mod$contents()$r, r)
  expect_identical(mod$contents()$initial_y, rep(1.0, length(r)))
})


test_that("user parameter validation", {
  gen <- odin({
    deriv(y) <- r
    initial(y) <- 1
    r <- user()
  })

  ## Honour all the options:
  expect_error(
    gen(user = list(r = 1, a = 1), unused_user_action = "stop"),
    "Unknown user parameters: a")
  expect_warning(
    gen(user = list(r = 1, a = 1), unused_user_action = "warning"),
    "Unknown user parameters: a")
  expect_message(
    gen(user = list(r = 1, a = 1), unused_user_action = "message"),
    "Unknown user parameters: a")
  expect_silent(
    gen(user = list(r = 1, a = 1), unused_user_action = "ignore"))

  ## Sensible error message for invalid option
  expect_error(
    gen(user = list(r = 1, a = 1), unused_user_action = "other"),
    "Unknown user parameters: a (and invalid value for unused_user_action)",
    fixed = TRUE)

  ## Inherit action from option
  with_options(
    list(odin.unused_user_action = "message"),
    expect_message(
      gen(user = list(r = 1, a = 1)),
      "Unknown user parameters: a"))

  ## Override option
  with_options(
    list(odin.unused_user_action = "message"),
    expect_error(
      gen(user = list(r = 1, a = 1), unused_user_action = "error"),
      "Unknown user parameters: a"))

  ## System default:
  with_options(
    list(odin.unused_user_action = NULL),
    expect_warning(
      gen(user = list(r = 1, a = 1)),
      "Unknown user parameters: a"))

  ## set_user:
  mod <- gen(r = 1)
  expect_silent(
    mod$set_user(user = list(x = 1), unused_user_action = "ignore"))
  expect_error(
    mod$set_user(user = list(x = 1), unused_user_action = "error"),
    "Unknown user parameters: x")
})
