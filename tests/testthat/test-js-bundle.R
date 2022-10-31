context("bundle")

test_that("bundle works", {
  skip_if_no_js()
  code <- c("deriv(N) <- r * N * (1 - N / K)",
            "initial(N) <- N0",
            "N0 <- user(1)",
            "K <- 100",
            "r <- user()")
  bundle <- odin_js_bundle(code)

  expect_false(bundle$is_discrete)
  expect_equal(bundle$support_file, "odin.js")

  t0 <- 0
  t1 <- 10
  tn <- 11

  res <- call_odin_bundle_continuous(bundle, list(r = 0.5), t0, t1, tn)

  t <- seq(t0, t1, length.out = tn)
  sol <- with(list(K = 100, r = 0.5, y0 = 1),
              K / (1 + (K / y0 - 1) * exp(-r * t)))

  expect_equal(res$x, t)
  expect_equal(res$values$name, "N")
  expect_equal(res$values$y[[1]], sol, tolerance = 1e-6)
})


test_that("include interpolate", {
  skip_if_no_js()
  code <- c("deriv(y) <- pulse",
            "initial(y) <- 0",
            "pulse <- interpolate(tp, zp, 'constant')",
            "tp[] <- user()",
            "zp[] <- user()",
            "dim(tp) <- user()",
            "dim(zp) <- user()",
            "output(p) <- pulse")

  bundle <- odin_js_bundle(code)

  t0 <- 0
  t1 <- 3
  tn <- 301

  tp <- c(0, 1, 2)
  zp <- c(0, 1, 0)
  user <- list(tp = tp, zp = zp)
  res <- call_odin_bundle_continuous(bundle, user, t0, t1, tn)

  tt <- seq(t0, t1, length.out = tn)

  expect_equal(res$values$name, c("y", "p"))
  zz <- ifelse(tt < 1, 0, ifelse(tt > 2, 1, tt - 1))
  expect_equal(res$values$y[[1]], zz, tolerance = 2e-5)
})


test_that("include sum", {
  skip_if_no_js()
  code <- c("deriv(y[]) <- r[i] * y[i]",
            "initial(y[]) <- 1",
            "r[] <- 0.1",
            "dim(r) <- 3",
            "dim(y) <- 3",
            "tot <- sum(y)",
            "output(ytot) <- tot",
            "output(y2[]) <- y[i] * 2",
            "dim(y2) <- length(y)")

  bundle <- odin_js_bundle(code)

  t0 <- 0
  t1 <- 10
  tn <- 101

  res <- call_odin_bundle_continuous(bundle, NULL, t0, t1, tn)
  expect_equal(
    res$values$name,
    c("y[1]", "y[2]", "y[3]", "ytot", "y2[1]", "y2[2]", "y2[3]"))

  y <- list_to_matrix(res$values$y[1:3])
  ytot <- res$values$y[[4]]
  y2 <- list_to_matrix(res$values$y[5:7])

  expect_equal(ytot, rowSums(y))
  expect_equal(y2, y * 2)
})


## Taken from the odin docs, generalised Lotka-Volterra.  This
## includes a partial sum but *not* a complete sum, which may trigger
## a failure to include the sum support.
test_that("include fancy sum", {
  skip_if_no_js()
  code <- c(
    "deriv(y[]) <- r[i] * y[i] * (1 - sum(ay[i, ]))",
    "initial(y[]) <- y0[i]",
    "y0[] <- user()",
    "r[] <- user()",
    "a[,] <- user()",
    "ay[,] <- a[i, j] * y[j]",
    "dim(r) <- user()",
    "n_spp <- length(r)",
    "dim(y) <- n_spp",
    "dim(y0) <- n_spp",
    "dim(a) <- c(n_spp, n_spp)",
    "dim(ay) <- c(n_spp, n_spp)")

  bundle <- odin_js_bundle(code)

  user <- list(r = c(1.00, 0.72, 1.53, 1.27),
               a = rbind(c(1.00, 1.09, 1.52, 0.00),
                         c(0.00, 1.00, 0.44, 1.36),
                         c(2.33, 0.00, 1.00, 0.47),
                         c(1.21, 0.51, 0.35, 1.00)),
               y0 = c(0.3013, 0.4586, 0.1307, 0.3557))

  t0 <- 0
  t1 <- 50
  tn <- 101

  res <- call_odin_bundle_continuous(bundle, user, t0, t1, tn)

  tt <- seq(t0, t1, length.out = tn)
  cmp <- odin::odin_(code, target = "r")$new(user = user)$run(tt)

  expect_equal(res$values$name, c("y[1]", "y[2]", "y[3]", "y[4]"))

  expect_equal(list_to_matrix(res$values$y),
               unname(cmp[, -1]), tolerance = 1e-5)
})


test_that("simple discrete model in a bundle", {
  skip_if_no_js()
  ## Not using the rng so that it's easy to push around:
  code <- c(
    "initial(x) <- 0",
    "update(x) <- x + r",
    "r <- user()")

  ## TODO: why does this force loading pkgload, that seems stupid and
  ## unneeded.
  bundle <- odin_js_bundle(code)

  expect_true(bundle$is_discrete)
  expect_equal(bundle$support_file, "dust.js")

  t0 <- 0
  t1 <- 20
  dt <- 1
  np <- 3

  res <- call_odin_bundle_discrete(bundle, list(r = 0.5), t0, t1, dt, np)
  expect_setequal(names(res), c("x", "values"))
  expect_equal(res$x, seq(t0, t1, dt))
  expect_equal(res$values$mode, "Deterministic")
  expect_equal(res$values$name, "x")
  expect_equal(res$values$y, list(seq(0, by = 0.5, length.out = 21)))
})


test_that("simple stochastic model in a bundle", {
  skip_if_no_js()
  code <- c(
    "initial(x) <- 0",
    "update(x) <- x + norm_rand()")

  bundle <- odin_js_bundle(code)

  expect_true(bundle$is_discrete)
  expect_equal(bundle$support_file, "dust.js")

  t0 <- 0
  t1 <- 20
  dt <- 0.5
  np <- 3

  res <- call_odin_bundle_discrete(bundle, NULL, t0, t1, dt, np)
  expect_setequal(names(res), c("x", "values"))
  expect_equal(res$x, seq(t0, t1, dt))
  expect_equal(res$values$mode, rep(c("Individual", "Mean"), c(np, 1)))
  expect_equal(res$values$name, rep("x", 4))
  expect_length(res$values$y, 4)
  expect_equal(
    rowMeans(matrix(unlist(res$values$y[1:3]), ncol = 3)),
    res$values$y[[4]])
})
