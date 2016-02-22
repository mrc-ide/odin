context("examples")

test_that("deSolve implementations work", {
  re <- "([[:alnum:]]+)_bm\\.txt$"
  files <- dir("examples", re)
  base <- sub(re, "\\1", files)
  filename <- sprintf("examples/%s_deSolve.R", base)
  for (i in which(file.exists(filename))) {
    b <- base[[i]]
    mod <- source1(filename[[i]])
    res <- run_model(mod, seq_range(mod$t, 300))
    expect_equal_to_reference(res, sprintf("examples/%s_deSolve.rds", b))
    if (FALSE) {
      j <- seq_len(attr(res, "lengthvar")[[1L]]) + 1L
      if (b == "array") {
        matplot(res[, 1], res[, j], col=rep(1:3, each=5), lty=1:5, type="l")
      } else {
        matplot(res[, 1], res[, j], type="l", lty=1)
      }
    }
  }
})

test_that("odin implementations work", {
  re <- "([[:alnum:]]+)_deSolve\\.R$"
  files <- dir("examples", re)
  base <- sub(re, "\\1", files)
  test <- intersect(c("lorenz", "sir", "seir", "array"), base)

  for (b in test) {
    filename_d <- sprintf("examples/%s_deSolve.R", b)
    filename_o <- sprintf("examples/%s_odin.R", b)
    ode <- if (b == "seir") deSolve::dede else deSolve::ode

    mod <- source1(filename_d)
    t <- seq_range(mod$t, 300)
    t0 <- mod$t[[1L]]

    dat <- odin_parse(filename_o)
    path <- odin_generate(dat, dest=tempfile(b, fileext=".c"))
    dll <- compile(path)

    ptr <- .Call("r_odin_create", NULL, PACKAGE=dll)
    expect_is(ptr, "externalptr")

    init <- .Call("r_odin_initialise", ptr, t0, PACKAGE=dll)
    expect_equal(init, unname(mod$initial(t0)))

    deriv_c <- .Call("r_odin_deriv", ptr, init, t0, PACKAGE=dll)
    deriv_r <- mod$derivs(t0, init)[[1]]
    expect_equal(deriv_c, deriv_r)

    tol <- if (b == "seir") 1e-7 else 1e-9

    res_r <- run_model(mod, t)
    res_c <- ode(init, t, "odin_ds_derivs", ptr,
                 initfunc="odin_ds_initmod", dllname=dll)
    expect_equal(res_c, res_r, check.attributes=FALSE, tolerance=tol)
  }
})
