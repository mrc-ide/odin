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
  re <- "([[:alnum:]]+)_odin\\.R$"
  files <- dir("examples", re)
  base <- sub(re, "\\1", files)
  test <- intersect(c("lorenz", "sir", "seir", "array", "array_2d"), base)

  for (b in test) {
    if (b == "array_2d") {
      filename_d <- "examples/array_deSolve.R"
    } else {
      filename_d <- sprintf("examples/%s_deSolve.R", b)
    }
    filename_o <- sprintf("examples/%s_odin.R", b)
    ode <- if (b == "seir") deSolve::dede else deSolve::ode

    mod <- source1(filename_d)
    t <- seq_range(mod$t, 300)
    t0 <- mod$t[[1L]]

    dat <- odin_parse(filename_o)
    path <- odin_generate(dat)
    dll <- compile(path, verbose=FALSE)
    info <- odin_dll_info(basename_no_ext(dll), dll)

    ptr <- .Call(info[["create"]], NULL)
    expect_is(ptr, "externalptr")

    init <- .Call(info[["init"]], ptr, t0)
    expect_equal(init, unname(mod$initial(t0)))

    output_len <- attr(init, "output_len")

    deriv_c <- .Call(info[["deriv"]], ptr, t0, init)
    deriv_r <- mod$derivs(t0, init)
    expect_equal(deriv_c, deriv_r[[1L]], check.attributes=FALSE)

    if (is.null(output_len)) {
      expect_null(attr(deriv_c, "output"))
    } else {
      ## The check.attributes is necessary because otherwise testthat
      ## gives entirely meaningless error messages on attribute
      ## differences (as it looks for differences in the values
      ## themselves).
      expect_equal(attr(deriv_c, "output", exact=TRUE), deriv_r[[2L]],
                   check.attributes=FALSE)
    }

    tol <- if (b == "seir") 1e-7 else 1e-9
    nout <- if (is.null(output_len)) 0L else output_len

    res_r <- run_model(mod, t)
    res_c <- ode(init, t, info[["ds_deriv"]], ptr,
                 initfunc=info[["ds_initmod"]], dllname=dll,
                 nout=nout)
    expect_equal(res_c, res_r, check.attributes=FALSE, tolerance=tol)
  }
  ## This might be needed to trigger finalisation of all models (which
  ## are all out of scope by now).
  gc()
})

test_that("nicer interface", {
  re <- "([[:alnum:]]+)_odin\\.R$"
  files <- dir("examples", re)
  base <- sub(re, "\\1", files)
  test <- intersect(c("lorenz", "sir", "seir", "array", "array_2d"), base)

  for (b in test) {
    if (b == "array_2d") {
      filename_d <- "examples/array_deSolve.R"
    } else {
      filename_d <- sprintf("examples/%s_deSolve.R", b)
    }
    filename_o <- sprintf("examples/%s_odin.R", b)

    mod_r <- source1(filename_d)
    t <- seq_range(mod_r$t, 300)
    t0 <- mod_r$t[[1L]]

    gen <- odin(filename_o, tempdir(), verbose=FALSE)
    expect_is(gen, "R6ClassGenerator")
    mod_c <- gen$new()
    expect_is(mod_c, "ode_system")

    expect_equal(mod_c$init,
                 if (mod_c$has_delay) NULL else unname(mod_r$initial(t0)))

    output_len <- sum(mod_c$output_order)

    if (mod_c$has_delay) {
      expect_error(mod_c$deriv(t0, mod_c$init), "not supported in delay")
    } else {
      deriv_c <- mod_c$deriv(t0, mod_c$init)
      deriv_r <- mod_r$derivs(t0, mod_c$init)
      expect_equal(deriv_c, deriv_r[[1L]], check.attributes=FALSE)

      if (output_len == 0L) {
        expect_null(attr(deriv_c, "output"))
      } else {
        ## The check.attributes is necessary because otherwise testthat
        ## gives entirely meaningless error messages on attribute
        ## differences (as it looks for differences in the values
        ## themselves).
        expect_equal(attr(deriv_c, "output", exact=TRUE), deriv_r[[2L]],
                     check.attributes=FALSE)
      }
    }

    tol <- if (b == "seir") 1e-7 else 1e-9

    res_r <- run_model(mod_r, t)
    res_c <- mod_c$run(t)
    expect_equal(res_c, res_r, check.attributes=FALSE, tolerance=tol)
  }
  gc()
})
