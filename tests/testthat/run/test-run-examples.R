context("run: %TARGET%: examples")

## TODO: this should all be rewritten

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

    has_delay <- b %in% c("seir", "seir_array")

    gen <- odin(filename_o)
    ## NOTE: this is a bit ugly; I'm really not sure what the right
    ## thing to do here is, but it might be to add an R6 option to
    ## odin() that would return the class (for use with inheritence
    ## etc) rather that the generating function here.
    mod_c <- gen()
    expect_is(mod_c, "odin_model")

    ## expect_equal(mod_c$init,
    ##              if (has_delay) NULL else unname(mod_r$initial(t0)))
    expect_equivalent(mod_c$initial(t0), unname(mod_r$initial(t0)))

    priv <- r6_private(mod_c)
    has_output <- priv$n_out > 0

    deriv_c <- mod_c$deriv(t0, mod_c$initial(t0))
    deriv_r <- mod_r$derivs(t0, mod_c$initial(t0))
    expect_equivalent(deriv_c, deriv_r[[1L]])

    if (has_output) {
      ## Using expect_equivalent to skip attributes is necessary
      ## because otherwise testthat gives entirely meaningless error
      ## messages on attribute differences (as it looks for
      ## differences in the values themselves).
      expect_equivalent(attr(deriv_c, "output", exact = TRUE), deriv_r[[2L]])
    } else {
      expect_null(attr(deriv_c, "output"))
    }

    ## These tolerances work for me locally on OSX, Windows and Linux,
    ## and on travis (seem different on 32bit windows)
    tol <- switch(b, seir = 1e-7, seir_array = 6e-7, 1e-9)

    res_r <- run_model(mod_r, t)
    res_c <- mod_c$run(t)
    if (!on_appveyor() && !on_cran()) {
      expect_equivalent(res_c, res_r, tolerance = tol)
    }

    y <- mod_c$transform_variables(res_c)
    expect_is(y, "list")

    if (has_output) {
      order <- c(priv$variable_order, priv$output_order)
    } else {
      order <- priv$variable_order
    }
    expect_equal(names(y), c(TIME, names(order)))
    is_array <- c(FALSE, !vlapply(order, is.null))
    len <- vnapply(order, function(x) if (is.null(x)) 1 else prod(x))

    expect_equal(lengths(y), length(t) * c(setNames(1, TIME), len))
    expect_true(all(viapply(y[!is_array], function(x) is.null(dim(x)))))

    expect_true(all(viapply(y[!is_array], function(x) is.null(dim(x)))))
    expect_true(all(viapply(y[is_array], function(x) !is.null(dim(x)))))
    for (i in which(is_array)) {
      expect_equal(dim(y[[i]]), c(length(t), order[[i - 1L]]))
    }
  }
  ## This might be needed to trigger finalisation of all models (which
  ## are all out of scope by now).
  gc()
})


test_that("user arrays", {
  ## In the first version we have constant sized arrays:
  gen1 <- odin("examples/array_odin.R")
  gen2 <- odin("examples/array_odin_user.R")

  mod1 <- gen1()
  age_width <- mod1$contents()$age_width

  expect_error(gen2(age_width[-1L]), "Expected length 5 value for age_width")
  expect_error(gen2(NULL), "Expected a value for 'age_width'")
  expect_error(gen2(numeric(0)), "Expected length 5 value for age_width")
  expect_error(gen2(rep(age_width, 2)),
               "Expected length 5 value for age_width")

  mod2 <- gen2(age_width)
  expect_equal(mod2$contents(), mod1$contents())

  t <- seq(0, 100, length.out = 101)
  res1 <- mod1$run(t)
  res2 <- mod2$run(t)
  expect_equal(res1, res2)

  ## User _sized_ arrays.
  gen3 <- odin("examples/array_odin_user2.R")
  mod3 <- gen3(age_width)

  dat3 <- mod3$contents()
  dat1 <- mod1$contents()
  expect_true(setequal(names(dat1), names(dat3)))
  expect_equal(dat3[names(dat1)], dat1)

  ## Now, let's set some different parameters here and check enforcement:
  age_width2 <- c(age_width, 365 * 25)
  expect_error(gen3(age_width2), "Expected length 5 value for age_width")
  expect_error(gen3(age_width, N_age = 6L),
               "Expected length 6 value for age_width")
  mod3 <- gen3(age_width2, N_age = 6L)
  expect_equal(mod3$contents()$age_width, age_width2)
  expect_equal(length(mod3$contents()$initial_R), length(age_width2))

  res3 <- mod3$run(t)
  tmp1 <- mod1$transform_variables(res1)
  tmp3 <- mod3$transform_variables(res3)
  expect_equal(tmp1$S[, 1], tmp3$S[, 1], tolerance = 1e-6)

  ## All in; this one is driven by a variable sized array.
  gen4 <- odin("examples/array_odin_user3.R")
  mod4 <- gen4(age_width)

  dat4 <- mod4$contents()
  expect_true(setequal(names(dat1), names(dat4)))
  expect_equal(dat4[names(dat1)], dat1)

  res4 <- mod4$run(t)
  expect_equal(res4, res1)

  ## Ideally, these tests will be run with gctorture on, as there is
  ## some nasty memory management going on behind the scenes that
  ## needs to work correctly.  However, it runs _very_ slowly with
  ## this on, and it does seem to work correctly at the moment.
  ##
  ## > prev <- gctorture(TRUE)
  ## > on.exit(gctorture(prev))
  mod4$set_user(age_width = age_width2)
  dat4.2 <- mod4$contents()
  expect_equal(dat4.2$age_width, age_width2)
  res4.2 <- mod4$run(t)
  expect_equal(res4.2, res3)
})


test_that("lv", {
  pars <- list(r = c(1.00, 0.72, 1.53, 1.27),
               a = rbind(c(1.00, 1.09, 1.52, 0.00),
                         c(0.00, 1.00, 0.44, 1.36),
                         c(2.33, 0.00, 1.00, 0.47),
                         c(1.21, 0.51, 0.35, 1.00)),
               y0 = c(0.3013, 0.4586, 0.1307, 0.3557))
  mod_r <- source1("examples/lv4_deSolve.R")
  invisible(mod_r$initial(pars = pars))
  gen <- odin("examples/lv4_odin.R")
  mod_c <- gen(user = pars)

  t <- seq_range(mod_r$t, 10000)
  t0 <- mod_r$t[[1L]]

  expect_is(mod_c, "odin_model")
  expect_equal(mod_c$initial(0), pars$y0)

  deriv_c <- mod_c$deriv(t0, mod_c$initial())
  deriv_r <- mod_r$derivs(t0, mod_c$initial())
  expect_equal(deriv_c, deriv_r[[1L]])

  res_r <- run_model(mod_r, t, pars)
  res_c <- mod_c$run(t)

  expect_equivalent(res_c, res_r)
  y <- mod_c$transform_variables(res_c)
  expect_is(y, "list")
  expect_equal(names(y), c(TIME, "y"))
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

    gen <- odin(filename_o)
    mod_ds <- gen()
    mod_dde <- gen(use_dde = TRUE)

    ## Looks good:
    expect_false(r6_private(mod_ds)$use_dde)
    expect_true(r6_private(mod_dde)$use_dde)

    ## Let's go.
    res_ds <- mod_ds$run(t)
    res_dde <- mod_dde$run(t)

    dimnames(res_ds) <- NULL
    attr(res_ds, "istate") <- NULL
    attr(res_ds, "rstate") <- NULL
    attr(res_ds, "type") <- NULL
    attr(res_ds, "lengthvar") <- NULL
    class(res_ds) <- "matrix"
    dimnames(res_dde) <- NULL
    attr(res_dde, "history") <- NULL

    ## The tolerances here are going to be not spectacular for some of
    ## the models, because Lorenz is chaotic...
    tol <- switch(b, lorenz = 1e-3, seir = 1e-5, seir_array = 6e-5,
                  array = 1e-5, array_2d = 1e-5, 1e-6)
    expect_equal(res_ds, res_dde, tolerance = tol)
  }
})
