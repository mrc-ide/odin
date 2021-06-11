context("package")

test_that("generate package", {
  skip_on_windows_gha()
  files <- sprintf("examples/%s_odin.R", ODIN_TO_TEST[1:2])
  res <- odin_create_package("example", files)
  on.exit(res$cleanup())

  mod <- res$env$lorenz_odin$new()
  cmp <- source1("examples/lorenz_deSolve.R")

  expect_setequal(dir(file.path(res$path, "src"), pattern = "\\.c$"),
                  c("odin.c", "registration.c"))
  expect_equal(dir(file.path(res$path, "R"), pattern = "\\.R$"), "odin.R")

  t <- seq(0, 10, length.out = 100)
  y_c <- mod$run(t)
  y_r <- run_model(cmp, t)
  expect_equivalent(y_c[, 2:4], y_r[, 2:4])

  ## Provides a loose check that the ir/coef bits work:
  expect_setequal(coef(res$env$sir)$name, c("I0", "beta"))

  ## Check we can get the ir out:
  expect_is(odin_ir(res$env$lorenz_odin), "json")
  expect_is(odin_ir(mod), "json")

  res$cleanup()
  on.exit()
})

test_that("interpolation", {
  skip_on_windows_gha()
  res <- odin_create_package("interpolation", "examples/interpolate_odin.R")
  on.exit(res$cleanup())

  flux_t <- c(1, 11, 21, 41, 73, 83, 93, 103, 113, 123, 133, 143, 153,
              163, 173, 183, 194, 204, 214, 224, 234, 244, 254, 264,
              274, 284, 294, 304, 315, 325, 335, 345, 355, 365)
  flux_y <- c(0.654, 0.167, 0.06, 0.07, 0.277, 0.186, 0.14, 0.255, 0.231,
              0.309, 1.127, 1.923, 1.091, 1.001, 1.691, 1.404, 1.226, 0.767,
              0.893, 0.737, 0.772, 0.726, 0.624, 0.439, 0.168, 0.28, 0.202,
              0.193, 0.286, 0.599, 1.889, 0.996, 0.681, 1.135)
  k <- 0.01
  C0 <- mean(approx(flux_t, flux_y, xout = 1:365)$y) / k
  mod <- res$env$interpolate_odin$new(kk = k, C0 = C0, flux_t = flux_t,
                                      flux_y = flux_y)

  t <- seq(1, 365)

  cmp <- source1("examples/interpolate_deSolve.R")
  pars <- list(flux_t = flux_t, flux_y = flux_y, k = k)
  y_r <- run_model(cmp, t, pars, tcrit = max(t))

  y_c <- mod$run(t, tcrit = max(t))

  ## On appveyor, these don't agree, and I can't replicate on a local
  ## windows machine.
  if (!on_appveyor() && !on_cran()) {
    expect_equal(y_c[, 2], y_r[, 2])
  }
})


test_that("ring", {
  skip_on_windows_gha()
  path <- "pkg/inst/odin/discretedelay.R"
  res <- odin_create_package("discretedelay", path)
  on.exit(res$cleanup())

  mod <- res$env$discretedelay$new()
  tt <- seq(0:10)
  yy <- mod$run(tt)
  expect_equal(yy[, "y"], c(1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144))
})


test_that("user_c", {
  skip_on_windows_gha()
  path <- c("pkg/inst/odin/pulse.R", "user_fns.c")
  res <- odin_create_package("pulse", path)
  on.exit(res$cleanup())

  mod <- res$env$pulse$new()
  t <- seq(0, 3, length.out = 301)
  y <- mod$run(t)

  expect_equal(y[, 3L], as.numeric(t >= 1 & t < 2))
  cmp <- -1 + t
  cmp[t < 1] <- 0
  cmp[t > 2] <- 1
  expect_equal(y[, 2L], cmp, tolerance = 1e-5)
})


## This could be improved:
test_that("pathalogical user c", {
  tmp <- file.path(tempdir(), "pulse2.R")
  txt <- sub("user_fns.c", "user_fns2.c", readLines("pkg/inst/odin/pulse.R"))
  writeLines(txt, tmp)
  path <- c("pkg/inst/odin/pulse.R", tmp, c("user_fns.c", "user_fns2.c"))
  expect_error(
    odin_create_package("pulse", path),
    "Duplicated entries in included C support not allowed")
})


test_that("allow reuse of user eqs", {
  skip_on_windows_gha()
  tmp <- file.path(tempdir(), "pulse2.R")
  txt <- file.copy("pkg/inst/odin/pulse.R", tmp, overwrite = TRUE)
  path <- c("pkg/inst/odin/pulse.R", tmp, "user_fns.c")
  res <- odin_create_package("pulse", path)

  t <- seq(0, 3, length.out = 301)
  expect_equal(
    res$env$pulse$new()$run(t),
    res$env$pulse2$new()$run(t))
})

test_that("error cases", {
  name <- "example"
  pkg <- file.path(tempfile(), name)
  dir.create(pkg, FALSE, TRUE)
  expect_error(odin_package(pkg), "Did not find package at")

  for (f in c("DESCRIPTION", "NAMESPACE")) {
    writeLines(sprintf(readLines(file.path("pkg", f)), name),
               file.path(pkg, f))
  }
  expect_error(odin_package(pkg), "Did not find inst/odin within your package")
  dir.create(file.path(pkg, "inst", "odin"), FALSE, TRUE)
  expect_error(odin_package(pkg), "Did not find any files in inst/odin")

  desc <- file.path(pkg, "DESCRIPTION")
  tmp <- tolower(readLines(desc))
  writeLines(tmp, desc)
  expect_error(odin_package(pkg),
               "Failed to get package name from DESCRIPTION")
})


## Same as the example:
test_that("example package", {
  skip_on_cran()
  skip_on_windows_gha()
  path <- tempfile()
  dir.create(path)

  src <- system.file("examples/package", package = "odin", mustWork = TRUE)
  file.copy(src, path, recursive = TRUE)
  pkg <- file.path(path, "package")

  odin_package(pkg)
  res <- build_package(pkg, FALSE)
  on.exit(res$cleanup())

  expect_is(res$env$lorenz, "odin_generator")
  mod <- res$env$lorenz$new()
  expect_equal(mod$initial(0), c(10, 1, 1))
  expect_equal(mod$deriv(0, c(10, 1, 1)),
               c(-90, 269, 22 / 3))
})


test_that("two sums example", {
  skip_on_cran()
  skip_on_windows_gha()

  path <- tempfile()
  dir.create(path)

  src <- system.file("examples/package", package = "odin", mustWork = TRUE)
  file.copy(src, path, recursive = TRUE)
  pkg <- file.path(path, "package")

  code <- c("deriv(x) <- sum(y[1, , ])",
            "initial(x) <- 0",
            "y[, , ] <- 1",
            "dim(y) <- c(2, 3, 4)")
  writeLines(code, file.path(pkg, "inst/odin/z.R"))

  odin_package(pkg)
  res <- build_package(pkg, FALSE)
  on.exit(res$cleanup())

  expect_is(res$env$z, "odin_generator")
  mod <- res$env$z$new()
  expect_equal(mod$initial(0), 0)
  expect_equal(mod$deriv(0, 0), 12)
})
