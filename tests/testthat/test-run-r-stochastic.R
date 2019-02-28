context("run: stochastic")

test_that("stochastic", {
  ## Here's a stochastic random walk:
  gen <- odin2({
    initial(x) <- 0
    update(x) <- x + norm_rand()
  }, verbose = TEST_VERBOSE)

  mod <- gen()
  tt <- 0:20
  set.seed(1)
  yy1 <- mod$run(tt)

  set.seed(1)
  cmp <- rnorm(length(tt) - 1L)
  expect_equal(cumsum(c(0, cmp)), yy1[, "x"])

  ## Repeatable
  set.seed(1)
  yy2 <- mod$run(tt)
  expect_equal(yy1, yy2)
})


## I'm not totally sure what the right call is here.  If I make a
## variable that is used only in the initial condition I do not want
## that repeatedly called during the run.
test_that("stochastic variables are time dependent", {
  gen <- odin2({
    v <- norm_rand() # this variable is implicitly time dependent.
    initial(x) <- 0
    update(x) <- x + v
  }, verbose = TEST_VERBOSE)

  mod <- gen()
  tt <- 0:20
  set.seed(1)
  yy1 <- mod$run(tt)

  set.seed(1)
  cmp <- rnorm(length(tt) - 1L)
  expect_equal(cumsum(c(0, cmp)), yy1[, "x"])
})


test_that("array stochastic variables are time dependent", {
  ## This checks that even in the absence of array indexing on the RHS
  ## array variables are set correctly when stochastic.
  gen <- odin2({
    initial(x[]) <- 0
    update(x[]) <- norm_rand()
    dim(x) <- 3
  }, verbose = TEST_VERBOSE)

  mod <- gen()
  tt <- 0:20
  set.seed(1)
  yy <- mod$run(tt)
  zz <- mod$transform_variables(yy)
  set.seed(1)
  cmp <- rbind(0, matrix(rnorm(3 * 20), 20, 3, TRUE))
  expect_equal(zz$x, cmp)
})


test_that("stochastic initial conditions don't get called every step", {
  ## There is quite a few nasty little conditions that are tested
  ## here.
  gen <- odin2({
    v <- norm_rand() # this variable is implicitly time dependent.
    initial(x) <- v
    update(x) <- x + 1
  }, verbose = TEST_VERBOSE)

  cmp <- .Random.seed
  mod <- gen()
  expect_equal(.Random.seed, cmp)

  ## Initial conditions (why is $init even a member here?)
  expect_null(mod$init)

  ## Re-running the initial conditions gives different answers:
  x0 <- mod$initial(0L)
  expect_false(identical(.Random.seed, cmp))
  expect_true(mod$initial(0L) != x0)

  ## Run the model from scratch
  tt <- 0:20
  set.seed(1)
  yy1 <- mod$run(tt)
  z <- rnorm(1)

  ## First number drawn from distribution, leaving RNG moved forward
  ## by a single normal draw:
  set.seed(1)
  cmp <- rnorm(2)
  expect_equal(yy1[, "x"], cmp[[1]] + tt)
  expect_equal(z, cmp[[2]])

  ## Don't advance the seed if not hitting the initial conditions.
  cmp <- .Random.seed
  expect_equal(mod$run(tt, 0)[, "x"], as.numeric(0:20))
  expect_equal(mod$run(tt, 1)[, "x"], as.numeric(1:21))
  expect_equal(.Random.seed, cmp)
})


test_that("exotic stochastic functions", {
  gen <- odin2({
    initial(x) <- 0
    mu <- 1
    sd <- 2
    update(x) <- rnorm(mu, sd)
  }, verbose = TEST_VERBOSE)

  set.seed(1)
  mod <- gen()
  y <- mod$run(0:10)

  set.seed(1)
  expect_equal(y[-1, "x"], rnorm(10, 1, 2))
})


test_that("round & rbinom", {
  gen <- odin2({
    size <- user()
    p <- user()
    update(x) <- 0
    initial(x) <- rbinom(size, p)
  }, verbose = TEST_VERBOSE)

  mod <- gen(p = 1, size = 0.4)
  expect_equal(mod$initial(0), 0)
  mod$set_user(p = 1, size = 1.7)
  expect_equal(mod$initial(0), 2)
})


test_that("mutlinomial", {
  skip("library support")
  ## This is just a check that these compile and run
  sir1 <- odin2("stochastic/sir_discrete.R", verbose = TEST_VERBOSE)
  sir2 <- odin2("stochastic/sir_discrete_stochastic.R", verbose = TEST_VERBOSE)
  sir3 <- odin2("stochastic/sir_discrete_stochastic2.R", verbose = TEST_VERBOSE)
  sir4 <- odin2("stochastic/sir_discrete_stochastic_multi.R",
                verbose = TEST_VERBOSE)

  mod1 <- sir1()
  mod2 <- sir2()
  mod3 <- sir3()
  mod4 <- sir4()

  t <- 0:100
  y1 <- mod1$run(t)
  y2 <- mod2$run(t)
  y3 <- mod3$run(t)
  y4 <- mod4$run(t)

  ## TODO: these need real tests!  At the moment I just want to
  ## confirm that they run.
  expect_true(TRUE)
})


test_that("replicate: scalar", {
  ## TODO: this will be a nice version to try and benchmark the dde
  ## overheads I think...
  gen <- odin2({
    initial(x) <- 0
    update(x) <- x + norm_rand()
  }, verbose = TEST_VERBOSE)
  m <- gen()
  tt <- 0:50
  res <- m$run(tt, replicate = 100)
  yy <- m$transform_variables(res)
  expect_equal(names(yy), c("t", "x"))
  expect_equal(yy[[1]], tt)
  expect_equal(dim(yy[[2]]), c(51, 100))
  expect_equal(yy[[1]], res[, 1, 1])
  expect_equal(yy[[2]], res[, 2, ])
})


test_that("replicate: array", {
  gen <- odin2({
    initial(x) <- 0
    initial(y[]) <- 0
    update(x) <- x + norm_rand()
    update(y[]) <- y[i] + norm_rand() / 2
    dim(y) <- 3
  }, verbose = TEST_VERBOSE)
  m <- gen()

  tt <- 0:20
  res <- m$run(tt, replicate = 30)
  yy <- m$transform_variables(res)
  expect_equal(names(yy), c("t", "x", "y"))
  expect_equal(yy[[1]], tt)
  expect_equal(dim(yy[[2]]), c(21, 30))
  expect_equal(dim(yy[[3]]), c(21, 3, 30))
  expect_equal(yy[[1]], res[, 1, 1])
  expect_equal(yy[[2]], res[, 2, ])
  expect_equal(yy[[3]], unname(res[, 3:5, ]))
})
