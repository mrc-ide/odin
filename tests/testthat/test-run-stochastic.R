context("run: stochastic")

test_that_odin("stochastic", {
  ## Here's a stochastic random walk:
  gen <- odin({
    initial(x) <- 0
    update(x) <- x + norm_rand()
  })

  mod <- gen$new()
  tt <- 0:20
  set.seed(1)
  yy1 <- mod$run(tt)

  set.seed(1)
  if (mod$engine() == "js") {
    cmp <- model_random_numbers(mod, "randomNormal", length(tt) - 1L)
  } else {
    cmp <- rnorm(length(tt) - 1L)
  }
  expect_equal(cumsum(c(0, cmp)), yy1[, "x"])

  ## Repeatable
  set.seed(1)
  yy2 <- mod$run(tt)
  expect_equal(yy1, yy2)
})


## I'm not totally sure what the right call is here.  If I make a
## variable that is used only in the initial condition I do not want
## that repeatedly called during the run.
test_that_odin("stochastic variables are time dependent", {
  gen <- odin({
    v <- norm_rand() # this variable is implicitly time dependent.
    initial(x) <- 0
    update(x) <- x + v
  })

  mod <- gen$new()
  tt <- 0:20
  set.seed(1)
  yy1 <- mod$run(tt)

  set.seed(1)
  if (mod$engine() == "js") {
    cmp <- model_random_numbers(mod, "randomNormal", length(tt) - 1L)
  } else {
    cmp <- rnorm(length(tt) - 1L)
  }
  expect_equal(cumsum(c(0, cmp)), yy1[, "x"])
})


test_that_odin("array stochastic variables are time dependent", {
  ## This checks that even in the absence of array indexing on the RHS
  ## array variables are set correctly when stochastic.
  gen <- odin({
    initial(x[]) <- 0
    update(x[]) <- norm_rand()
    dim(x) <- 3
  })

  mod <- gen$new()
  tt <- 0:20
  set.seed(1)
  yy <- mod$run(tt)
  zz <- mod$transform_variables(yy)

  set.seed(1)
  if (mod$engine() == "js") {
    cmp <- model_random_numbers(mod, "randomNormal", 3 * 20)
  } else {
    cmp <- rnorm(3 * 20)
  }
  expect_equal(zz$x, rbind(0, matrix(cmp, 20, 3, TRUE)))
})


test_that_odin("stochastic initial conditions don't get called every step", {
  skip_for_target("js") # dust does not support this, so dust.js also does not
  ## There is quite a few nasty little conditions that are tested
  ## here.
  gen <- odin({
    v <- norm_rand() # this variable is implicitly time dependent.
    initial(x) <- v
    update(x) <- x + 1
  })

  cmp <- .Random.seed
  mod <- gen$new()

  if (mod$engine() != "js") {
    expect_equal(.Random.seed, cmp)
  }

  ## Re-running the initial conditions gives different answers:
  x0 <- mod$initial(0L)
  if (mod$engine() != "js") {
    expect_false(identical(.Random.seed, cmp))
  }
  expect_true(mod$initial(0L) != x0)

  ## Run the model from scratch
  tt <- 0:20
  set.seed(1)
  yy1 <- mod$run(tt)
  if (mod$engine() == "js") {
    z <- model_random_numbers(mod, "randomNormal", 1)
  } else {
    z <- rnorm(1)
  }

  ## First number drawn from distribution, leaving RNG moved forward
  ## by a single normal draw:
  set.seed(1)
  if (mod$engine() == "js") {
    cmp <- model_random_numbers(mod, "randomNormal", 2)
  } else {
    cmp <- rnorm(2)
  }
  expect_equal(yy1[, "x"], cmp[[1]] + tt)
  expect_equal(z, cmp[[2]])

  ## Don't advance the seed if not hitting the initial conditions.
  cmp <- .Random.seed
  expect_equal(mod$run(tt, 0)[, "x"], as.numeric(0:20))
  expect_equal(mod$run(tt, 1)[, "x"], as.numeric(1:21))
  if (mod$engine() != "js") {
    expect_equal(.Random.seed, cmp)
  }
})


test_that_odin("exotic stochastic functions", {
  gen <- odin({
    initial(x) <- 0
    mu <- 1
    sd <- 2
    update(x) <- rnorm(mu, sd)
  })

  mod <- gen$new()
  set.seed(1)
  y <- mod$run(0:10)

  set.seed(1)
  if (mod$engine() == "js") {
    cmp <- model_random_numbers(mod, "normal", 10, 1, 2)
  } else {
    cmp <- rnorm(10, 1, 2)
  }

  expect_equal(y[-1, "x"], cmp)
})


test_that_odin("round & rbinom", {
  gen <- odin({
    size <- user()
    p <- user()
    initial(x) <- 0
    update(x) <- rbinom(size, p)
  })

  mod <- gen$new(p = 1, size = 0.4)
  expect_equal(mod$update(0, 0), 0)
  mod$set_user(p = 1, size = 1.7)
  expect_equal(mod$update(0, 0), 2)
})


test_that_odin("multinomial", {
  skip_for_target("js") # needs work in random.js
  ## This is just a check that these compile and run
  sir1 <- odin("stochastic/sir_discrete.R")
  sir2 <- odin("stochastic/sir_discrete_stochastic.R")
  sir3 <- odin("stochastic/sir_discrete_stochastic2.R")
  sir4 <- odin("stochastic/sir_discrete_stochastic_multi.R")

  mod1 <- sir1$new()
  mod2 <- sir2$new()
  mod3 <- sir3$new()
  mod4 <- sir4$new()

  t <- 0:100
  y1 <- mod1$run(t)
  y2 <- mod2$run(t)
  y3 <- mod3$run(t)
  y4 <- mod4$run(t)

  ## TODO: these need real tests!  At the moment I just want to
  ## confirm that they run.
  expect_true(TRUE)
})


test_that_odin("replicate: scalar", {
  skip_for_target("js") # we'll probably remove this generally at some point
  ## TODO: this will be a nice version to try and benchmark the dde
  ## overheads I think...
  gen <- odin({
    initial(x) <- 0
    update(x) <- x + norm_rand()
  })
  m <- gen$new()
  tt <- 0:50
  res <- m$run(tt, replicate = 100)
  yy <- m$transform_variables(res)
  expect_equal(names(yy), c("t", "x"))
  expect_equal(yy[[1]], tt)
  expect_equal(dim(yy[[2]]), c(51, 100))
  expect_equal(yy[[1]], res[, 1, 1])
  expect_equal(yy[[2]], res[, 2, ])
})


test_that_odin("replicate: array", {
  skip_for_target("js") # probably will be removed later
  gen <- odin({
    initial(x) <- 0
    initial(y[]) <- 0
    update(x) <- x + norm_rand()
    update(y[]) <- y[i] + norm_rand() / 2
    dim(y) <- 3
  })
  m <- gen$new()

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


test_that_odin("low-level stochastics: norm_rand", {
  gen <- odin({
    initial(y) <- 0
    update(y) <- norm_rand()
  })
  m <- gen$new()

  tt <- 0:10
  set.seed(1)
  y <- m$run(tt)[-1, "y"]

  set.seed(1)
  if (m$engine() == "js") {
    cmp <- model_random_numbers(m, "randomNormal", 10)
  } else {
    cmp <- rnorm(10)
  }
  expect_equal(y, cmp)
})


test_that_odin("low-level stochastics: unif_rand", {
  gen <- odin({
    initial(y) <- 0
    update(y) <- unif_rand()
  })
  m <- gen$new()

  tt <- 0:10
  set.seed(1)
  y <- m$run(tt)[-1, "y"]

  set.seed(1)
  if (m$engine() == "js") {
    cmp <- model_random_numbers(m, "randomUniform", 10)
  } else {
    cmp <- runif(10)
  }

  expect_equal(y, cmp)
})


test_that_odin("low-level stochastics: exp_rand", {
  gen <- odin({
    initial(y) <- 0
    update(y) <- exp_rand()
  })
  m <- gen$new()

  tt <- 0:10
  set.seed(1)
  y <- m$run(tt)[-1, "y"]

  set.seed(1)
  if (m$engine() == "js") {
    cmp <- model_random_numbers(m, "randomExponential", 10)
  } else {
    cmp <- rexp(10)
  }
  expect_equal(y, cmp)
})


test_that_odin("rexp parametrisation", {
  gen <- odin({
    initial(y) <- 0
    update(y) <- rexp(10)
  })
  m <- gen$new()

  tt <- 0:10
  set.seed(1)
  y <- m$run(tt)[-1, "y"]

  set.seed(1)
  if (m$engine() == "js") {
    cmp <- model_random_numbers(m, "exponential", 10, 10)
  } else {
    cmp <- rexp(10, 10)
  }
  expect_equal(y, cmp)
})
