context("package")

test_that("generate package", {
  files <- sprintf("examples/%s_odin.R", ODIN_TO_TEST[1:2])
  res <- odin_create_package("example", files, verbose=FALSE)
  on.exit(res$cleanup())

  mod <- res$env$lorenz_odin()
  cmp <- source1("examples/lorenz_deSolve.R")

  t <- seq(0, 10, length.out=100)
  y_c <- mod$run(t)
  y_r <- run_model(cmp, t)
  expect_equal(y_c[, 2:4], y_r[, 2:4], check.attributes=FALSE)
})

test_that("interpolation", {
  res <- odin_create_package("interpolation", "examples/interpolate_odin.R",
                             verbose=FALSE)
  on.exit(res$cleanup())

  flux_t <- c(1, 11, 21, 41, 73, 83, 93, 103, 113, 123, 133, 143, 153,
              163, 173, 183, 194, 204, 214, 224, 234, 244, 254, 264,
              274, 284, 294, 304, 315, 325, 335, 345, 355, 365)
  flux_y <- c(0.654, 0.167, 0.06, 0.07, 0.277, 0.186, 0.14, 0.255, 0.231,
              0.309, 1.127, 1.923, 1.091, 1.001, 1.691, 1.404, 1.226, 0.767,
              0.893, 0.737, 0.772, 0.726, 0.624, 0.439, 0.168, 0.28, 0.202,
              0.193, 0.286, 0.599, 1.889, 0.996, 0.681, 1.135)
  k <- 0.01
  C0 <- mean(approx(flux_t, flux_y, xout=1:365)$y) / k
  mod <- res$env$interpolate_odin(kk=k, C0=C0, flux_t=flux_t, flux_y)

  t <- seq(1, 365)

  cmp <- source1("examples/interpolate_deSolve.R")
  pars <- list(flux_t=flux_t, flux_y=flux_y, k=k)
  y_r <- run_model(cmp, t, pars, tcrit=max(t))

  y_c <- mod$run(t, tcrit=max(t))

  expect_equal(y_c[, 2], y_r[, 2])
})
