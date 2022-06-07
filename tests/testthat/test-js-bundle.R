context("bundle")

test_that("bundle works", {
  code <- c("deriv(N) <- r * N * (1 - N / K)",
            "initial(N) <- N0",
            "N0 <- user(1)",
            "K <- 100",
            "r <- user()")
  bundle <- odin_js_bundle(code)

  t0 <- 0
  t1 <- 10
  tn <- 11

  res <- call_odin_bundle(bundle, list(r = 0.5), t0, t1, tn)

  t <- seq(t0, t1, length.out = tn)
  sol <- with(list(K = 100, r = 0.5, y0 = 1),
              K / (1 + (K / y0 - 1) * exp(-r * t)))

  expect_length(res, 1)
  expect_equal(res[[1]]$x, t)
  expect_equal(res[[1]]$y, sol, tolerance = 1e-6)
  expect_equal(res[[1]]$name, "N")
})


test_that("include interpolate", {
  testthat::skip("FIXME")
  code <- c("deriv(y) <- pulse",
            "initial(y) <- 0",
            "pulse <- interpolate(tp, zp, 'constant')",
            "tp[] <- user()",
            "zp[] <- user()",
            "dim(tp) <- user()",
            "dim(zp) <- user()",
            "output(p) <- pulse")

  res <- odin_js_bundle(code)

  ct <- V8::v8()
  invisible(ct$eval(res))

  tt <- seq(0, 3, length.out = 301)
  tp <- c(0, 1, 2)
  zp <- c(0, 1, 0)
  user <- list(tp = tp, zp = zp)
  yy <- call_odin_bundle(ct, "odin", user, tt)
  zz <- ifelse(tt < 1, 0, ifelse(tt > 2, 1, tt - 1))
  expect_equal(yy[, 2], zz, tolerance = 2e-5)
})


test_that("include sum", {
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

  res <- call_odin_bundle(bundle, NULL, t0, t1, tn)

  expect_length(res, 7)
  expect_equal(
    vcapply(res, "[[", "name"),
    c("y[1]", "y[2]", "y[3]", "ytot", "y2[1]", "y2[2]", "y2[3]"))

  y <- vapply(res[1:3], "[[", numeric(tn), "y")
  ytot <- res[[4]]$y
  y2 <- vapply(res[5:7], "[[", numeric(tn), "y")
  expect_equal(ytot, rowSums(y))
  expect_equal(y2, y * 2)
})


## Taken from the odin docs, generalised Lotka-Volterra.  This
## includes a partial sum but *not* a complete sum, which may trigger
## a failure to include the sum support.
test_that("include fancy sum", {
  ## This will error on the partial sums during model
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

  res <- call_odin_bundle(bundle, user, t0, t1, tn)

  tt <- seq(t0, t1, length.out = tn)
  cmp <- odin::odin_(code, target = "r")$new(user = user)$run(tt)

  expect_length(res, 4)
  yy <- vapply(res, "[[", numeric(tn), "y")

  expect_equal(yy, unname(cmp[, -1]), tolerance = 1e-5)
})


test_that("simple stochastic model in a bundle", {
  testthat::skip("FIXME")
  code <- c(
    "initial(x) <- 0",
    "update(x) <- x + norm_rand()")

  res <- odin_js_bundle(code)

  ct <- V8::v8()
  invisible(ct$eval(res))
  ct$call("setSeed", 1)
  tt <- 0:20
  yy <- call_odin_bundle(ct, "odin", NULL, tt)

  mod <- odin_(code, target = "js")$new()
  model_set_seed(mod, 1)
  cmp <- mod$run(tt)
  expect_equal(yy, cmp)
})
