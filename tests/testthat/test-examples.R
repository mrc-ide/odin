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

test_that("basic interface", {
  re <- "([[:alnum:]]+)_odin\\.R$"
  files <- dir("examples", re)
  base <- sub(re, "\\1", files)
  test <- intersect(ODIN_TO_TEST, base)

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

    gen <- odin(filename_o, verbose=TEST_VERBOSE)
    ## NOTE: this is a bit ugly; I'm really not sure what the right
    ## thing to do here is, but it might be to add an R6 option to
    ## odin() that would return the class (for use with inheritence
    ## etc) rather that the generating function here.
    expect_is(environment(gen)$cl, "R6ClassGenerator")
    mod_c <- gen()
    expect_is(mod_c, "ode_system")

    expect_equal(mod_c$init,
                 if (mod_c$has_delay) NULL else unname(mod_r$initial(t0)))
    expect_equal(mod_c$initial(t0), unname(mod_r$initial(t0)))

    if (mod_c$has_delay) {
      expect_error(mod_c$deriv(t0, mod_c$init), "not supported in delay")
    } else {
      deriv_c <- mod_c$deriv(t0, mod_c$init)
      deriv_r <- mod_r$derivs(t0, mod_c$init)
      expect_equal(deriv_c, deriv_r[[1L]], check.attributes=FALSE)

      if (mod_c$output_length == 0L) {
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

    tol <- switch(b, seir=1e-7, seir_array=6e-7, 1e-9)

    res_r <- run_model(mod_r, t)
    res_c <- mod_c$run(t)
    expect_equal(res_c, res_r, check.attributes=FALSE, tolerance=tol)

    y <- mod_c$transform_variables(res_c)
    expect_is(y, "list")

    if (mod_c$has_output) {
      order <- c(mod_c$variable_order, mod_c$output_order)
    } else {
      order <- mod_c$variable_order
    }
    expect_equal(names(y), c(names(order)))
    is_array <- !vlapply(order, is.null)
    len <- vnapply(order, function(x) if (is.null(x)) 1 else prod(x))

    expect_equal(lengths(y), length(t) * len)
    expect_true(all(viapply(y[!is_array], function(x) is.null(dim(x)))))

    expect_true(all(viapply(y[!is_array], function(x) is.null(dim(x)))))
    expect_true(all(viapply(y[is_array], function(x) !is.null(dim(x)))))
    for (i in which(is_array)) {
      expect_equal(dim(y[[i]]), c(length(t), order[[i]]))
    }
  }
  ## This might be needed to trigger finalisation of all models (which
  ## are all out of scope by now).
  gc()
})

test_that("user arrays", {
  ## In the first version we have constant sized arrays:
  gen1 <- odin("examples/array_odin.R", verbose=TEST_VERBOSE)
  gen2 <- odin("examples/array_odin_user.R", verbose=TEST_VERBOSE)

  mod1 <- gen1()
  age_width <- mod1$contents()$age_width

  expect_error(gen2(age_width[-1L]), "Expected length 5 value for age_width")
  expect_error(gen2(NULL), "Expected value for age_width")
  expect_error(gen2(numeric(0)), "Expected length 5 value for age_width")
  expect_error(gen2(rep(age_width, 2)),
               "Expected length 5 value for age_width")

  mod2 <- gen2(age_width)
  expect_equal(mod2$contents(), mod1$contents())

  t <- seq(0, 100, length.out=101)
  res1 <- mod1$run(t)
  res2 <- mod2$run(t)
  expect_equal(res1, res2)

  expect_equal(mod1$dim_stage, STAGE_CONSTANT)
  expect_equal(mod2$dim_stage, STAGE_CONSTANT)
  expect_equal(mod1$initial_stage, STAGE_USER)
  expect_equal(mod2$initial_stage, STAGE_USER)

  ## User *sized* arrays.
  gen3 <- odin("examples/array_odin_user2.R", verbose=TEST_VERBOSE)
  mod3 <- gen3(age_width)

  expect_equal(mod3$dim_stage, STAGE_USER)
  expect_equal(mod3$initial_stage, STAGE_USER)

  dat3 <- mod3$contents()
  dat1 <- mod1$contents()
  expect_true(setequal(names(dat1), names(dat3)))
  expect_equal(dat3[names(dat1)], dat1)

  ## Now, let's set some different parameters here and check enforcement:
  age_width2 <- c(age_width, 365 * 25)
  expect_error(gen3(age_width2), "Expected length 5 value for age_width")
  expect_error(gen3(age_width, N_age=6L),
               "Expected length 6 value for age_width")
  mod3 <- gen3(age_width2, N_age=6L)
  expect_equal(mod3$contents()$age_width, age_width2)
  expect_equal(length(mod3$contents()$initial_R), length(age_width2))

  res3 <- mod3$run(t)
  tmp1 <- mod1$transform_variables(res1)
  tmp3 <- mod3$transform_variables(res3)
  expect_equal(tmp1$S[, 1], tmp3$S[, 1], tolerance=1e-6)

  ## All in; this one is driven by a variable sized array.
  gen4 <- odin("examples/array_odin_user3.R", verbose=TEST_VERBOSE)
  mod4 <- gen4(age_width)

  dat4 <- mod4$contents()
  expect_true(setequal(names(dat1), names(dat4)))
  expect_equal(dat4[names(dat1)], dat1)

  res4 <- mod4$run(t)
  expect_equal(res4, res1)

  ## Ideally, these tests will be run with gctorture on, as there is
  ## some nasty memory management going on behind the scenes that
  ## needs to work correctly.  Howeever, it runs _very_ slowly with
  ## this on, and it does seem to work correctly at the moment.
  ##
  ##   prev <- gctorture(TRUE)
  ##   on.exit(gctorture(prev))
  mod4$set_user(age_width=age_width2)
  dat4.2 <- mod4$contents()
  expect_equal(dat4.2$age_width, age_width2)
  res4.2 <- mod4$run(t)
  expect_equal(res4.2, res3)
})

test_that("lv", {
  pars <- list(r=c(1.00, 0.72, 1.53, 1.27),
               a=rbind(c(1.00, 1.09, 1.52, 0.00),
                       c(0.00, 1.00, 0.44, 1.36),
                       c(2.33, 0.00, 1.00, 0.47),
                       c(1.21, 0.51, 0.35, 1.00)),
               y0=c(0.3013, 0.4586, 0.1307, 0.3557))
  mod_r <- source1("examples/lv4_deSolve.R")
  invisible(mod_r$initial(pars=pars))
  gen <- odin("examples/lv4_odin.R", verbose=TEST_VERBOSE)
  mod_c <- gen(user=pars)

  t <- seq_range(mod_r$t, 10000)
  t0 <- mod_r$t[[1L]]

  expect_is(mod_c, "ode_system")
  expect_equal(mod_c$init, pars$y0)

  deriv_c <- mod_c$deriv(t0, mod_c$init)
  deriv_r <- mod_r$derivs(t0, mod_c$init)
  expect_equal(deriv_c, deriv_r[[1L]])

  res_r <- run_model(mod_r, t, pars)
  res_c <- mod_c$run(t)

  expect_equal(res_c, res_r, check.attributes=FALSE)
  y <- mod_c$transform_variables(res_c)
  expect_is(y, "list")
  expect_equal(names(y), "y")
  expect_equal(dim(y$y), c(length(t), 4))
})

test_that("dde", {
  skip_if_not_installed("dde")

  re <- "([[:alnum:]]+)_odin\\.R$"
  files <- dir("examples", re)
  base <- sub(re, "\\1", files)
  test <- intersect(ODIN_TO_TEST, base)

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

    gen <- odin(filename_o, verbose=TEST_VERBOSE)
    mod_ds <- gen()
    mod_dde <- gen(dde=TRUE)

    ## Looks good:
    expect_false(mod_ds$use_dde)
    expect_true(mod_dde$use_dde)

    ## Correct function found:
    expect_equal(environmentName(environment(mod_ds$ode)), "deSolve")
    expect_equal(mod_dde$ode, dde::dopri)

    ## Let's go.
    res_ds <- mod_ds$run(t)
    res_dde <- mod_dde$run(t)

    dimnames(res_ds) <- NULL
    attr(res_ds, "istate") <- NULL
    attr(res_ds, "rstate") <- NULL
    attr(res_ds, "type") <- NULL
    class(res_ds) <- "matrix"
    dimnames(res_dde) <- NULL

    ## The tolerances here are going to be not spectacular for some of
    ## the models, because Lorenz is chaotic...
    tol <- switch(b, lorenz=1e-3, seir=1e-5, seir_array=6e-5,
                  array=1e-5, array_2d=1e-5, 1e-6)
    expect_equal(res_ds, res_dde, tolerance=tol)
  }
})
