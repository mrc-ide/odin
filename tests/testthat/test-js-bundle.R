context("bundle")

test_that("bundle works", {
  code <- c("deriv(N) <- r * N * (1 - N / K)",
            "initial(N) <- N0",
            "N0 <- user(1)",
            "K <- 100",
            "r <- user()")
  p <- tempfile()
  dir.create(p)
  filename <- file.path(p, "odin.R")
  writeLines(code, filename)

  res <- odin_js_bundle(filename)

  ## Keep unwanted bits out:
  txt <- readLines(res)
  expect_false(any(grepl("odinSum2", txt)))
  expect_false(any(grepl("interpolateCheckY", txt)))

  ct <- V8::v8()
  invisible(ct$source(res))

  t <- 0:10
  res <- call_odin_bundle(ct, "odin", list(r = 0.5), t)
  sol <- with(list(K = 100, r = 0.5, y0 = 1),
              K / (1 + (K / y0 - 1) * exp(-r * t)))

  expect_equal(res[, "t"], t)
  expect_equal(res[, "N"], sol, tolerance = 1e-6)
})


test_that("bundle fails with missing files", {
  path1 <- file.path(tempdir(), "myfile1.R")
  path2 <- file.path(tempdir(), "myfile2.R")
  expect_error(odin_js_bundle(path1), "File does not exist:")
  expect_error(odin_js_bundle(c(path1, path2)), "Files do not exist:")
})


test_that("unique model names", {
  path1 <- tempfile()
  path2 <- tempfile()

  code <- c("deriv(y) <- 1", "initial(y) <- 1", 'config(base) <- "test"')
  writeLines(code, path1)
  writeLines(code, path2)

  expect_error(odin_js_bundle(c(path1, path2)),
               "Duplicate model names: 'test'")
})


test_that("include interpolate", {
  code <- c("deriv(y) <- pulse",
            "initial(y) <- 0",
            "pulse <- interpolate(tp, zp, 'constant')",
            "tp[] <- user()",
            "zp[] <- user()",
            "dim(tp) <- user()",
            "dim(zp) <- user()",
            "output(p) <- pulse")

  p <- tempfile()
  dir.create(p)
  filename <- file.path(p, "odin.R")
  writeLines(code, filename)

  res <- odin_js_bundle(filename)

  ct <- V8::v8()
  invisible(ct$source(res))

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

  p <- tempfile()
  dir.create(p)
  filename <- file.path(p, "odin.R")
  writeLines(code, filename)
  res <- odin_js_bundle(filename)

  ct <- V8::v8()
  invisible(ct$source(res))
  tt <- seq(0, 10, length.out = 101)
  yy <- call_odin_bundle(ct, "odin", NULL, tt)
  res <- list(y = unname(yy[, 2:4]),
              ytot = unname(yy[, 5, drop = TRUE]),
              y2 = unname(yy[, 6:8]))
  expect_equal(res$ytot, rowSums(res$y))
  expect_equal(res$y2, res$y * 2)
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

  p <- tempfile()
  dir.create(p)
  filename <- file.path(p, "odin.R")
  writeLines(code, filename)
  res <- odin_js_bundle(filename)

  user <- list(r = c(1.00, 0.72, 1.53, 1.27),
               a = rbind(c(1.00, 1.09, 1.52, 0.00),
                         c(0.00, 1.00, 0.44, 1.36),
                         c(2.33, 0.00, 1.00, 0.47),
                         c(1.21, 0.51, 0.35, 1.00)),
               y0 = c(0.3013, 0.4586, 0.1307, 0.3557))

  ct <- V8::v8()
  invisible(ct$source(res))

  tt <- seq(0, 50, length.out = 101)
  yy <- call_odin_bundle(ct, "odin", user, tt)

  cmp <- odin::odin_(code, target = "r")$new(user = user)$run(tt)
  expect_equivalent(yy, cmp[], tolerance = 1e-5)
})


test_that("simple stochastic model in a bundle", {
  code <- c(
    "initial(x) <- 0",
    "update(x) <- x + norm_rand()")

  p <- tempfile()
  dir.create(p)
  filename <- file.path(p, "odin.R")
  writeLines(code, filename)
  res <- odin_js_bundle(filename)

  ct <- V8::v8()
  invisible(ct$source(res))
  ct$call("setSeed", 1)
  tt <- 0:20
  yy <- call_odin_bundle(ct, "odin", NULL, tt)

  mod <- odin_js_(code)$new()
  model_set_seed(mod, 1)
  cmp <- mod$run(tt)
  expect_equal(yy, cmp)
})
