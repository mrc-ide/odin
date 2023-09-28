test_that("Can parse with differentiable parameters", {
  ir <- odin_parse({
    initial(x) <- 1
    update(x) <- rnorm(x, 0.1)
    d <- data()
    compare(d) ~ normal(0, scale)
    scale <- user(differentiate = TRUE)
  })

  d <- ir_deserialise(ir)
  expect_true(d$features$has_derivative)
})


test_that("can't differentiate integer parameters", {
  expect_error(odin_parse({
    initial(x) <- 1
    update(x) <- rnorm(0, 0.1)
    d <- data()
    compare(d) ~ normal(x, scale)
    scale <- user(differentiate = TRUE, integer = TRUE)
  }),
  "Can't differentiate integer parameters\\s+scale <-")
})


test_that("can't differentiate without compare", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- rnorm(x, scale)
      scale <- user(differentiate = TRUE)
    }),
    "You need a compare expression to differentiate!\\s+scale <-")
})


test_that("can't differentiate continuous time models", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      deriv(x) <- 1
      d <- data()
      compare(d) ~ normal(x, scale)
      scale <- user(differentiate = TRUE)
    }),
    "Can't use differentiate with continuous time models\\s+scale <-")
})


test_that("can't differentiate models with arrays", {

  ir <- odin_parse({
    initial(x[]) <- 1
    update(x[]) <- rnorm(x[i], 1)
    dim(x) <- 5
    d <- data()
    compare(d) ~ normal(sum(x), scale)
    scale <- user(differentiate = TRUE)
  })

})


test_that("can differentiate nontrivial model", {
  withr::local_locale(c("LC_COLLATE" = "C"))
  ir <- odin_parse_("examples/sir_adjoint.R")
  d <- ir_deserialise(ir)

  expect_true(d$features$has_derivative)
  expect_setequal(names(d$derivative), c("parameters", "adjoint"))
  expect_equal(d$derivative$parameters, c("I0", "beta", "gamma"))
  expect_setequal(names(d$derivative$adjoint),
                  c("variables", "components"))

  expect_equal(
    d$derivative$adjoint$components$rhs$variables,
    c("S", "I", "R", "adjoint_I", "adjoint_R", "adjoint_S",
      "adjoint_cases_cumul", "adjoint_cases_inc", "adjoint_I0",
      "adjoint_beta", "adjoint_gamma"))
  expect_equal(
    d$derivative$adjoint$components$rhs$equations,
    c("adjoint_n_IR", "adjoint_n_SI", "adjoint_update_cases_cumul",
      "adjoint_update_cases_inc", "adjoint_update_I0", "N", "adjoint_p_IR",
      "adjoint_p_SI", "p_inf", "adjoint_p_inf", "adjoint_update_gamma",
      "p_SI", "adjoint_N", "adjoint_update_beta", "adjoint_update_I",
      "adjoint_update_R", "adjoint_update_S"))

  expect_equal(
    d$derivative$adjoint$components$compare$variables,
    c("cases_inc", "adjoint_S", "adjoint_I", "adjoint_R", "adjoint_cases_cumul",
      "adjoint_cases_inc", "adjoint_I0", "adjoint_beta", "adjoint_gamma"))
  expect_equal(
    d$derivative$adjoint$components$compare$equations,
    c("adjoint_compare_S", "adjoint_compare_I", "adjoint_compare_R",
      "adjoint_compare_cases_cumul", "adjoint_compare_cases_inc",
      "adjoint_compare_I0", "adjoint_compare_beta", "adjoint_compare_gamma"))

  expect_equal(
    d$derivative$adjoint$components$initial$variables,
    c("adjoint_S", "adjoint_I", "adjoint_R", "adjoint_cases_cumul",
      "adjoint_cases_inc", "adjoint_I0", "adjoint_beta", "adjoint_gamma"))
  expect_equal(
    d$derivative$adjoint$components$initial$equations,
    c("adjoint_initial_S", "adjoint_initial_I", "adjoint_initial_R",
      "adjoint_initial_cases_cumul", "adjoint_initial_cases_inc",
      "adjoint_initial_I0", "adjoint_initial_beta", "adjoint_initial_gamma"))

  ## Then some equations, these are much harder to check, and there
  ## are quite a lot of them.
  expect_length(d$equations, 53)

  expected <- list(
    adjoint_N = "-adjoint_p_inf * (beta * I) * dt/(N * N)",
    adjoint_n_IR = "-adjoint_I + adjoint_R",
    adjoint_n_SI = paste("adjoint_I + -adjoint_S + adjoint_cases_cumul +",
                         "adjoint_cases_inc"),
    adjoint_n_SI = paste("adjoint_cases_cumul + adjoint_cases_inc +",
                         "adjoint_I + -adjoint_S"),
    adjoint_p_inf = "adjoint_p_SI * exp(-p_inf)",
    adjoint_p_IR = "adjoint_n_IR * I",
    adjoint_p_SI = "adjoint_n_SI * S",
    adjoint_update_cases_cumul = "adjoint_cases_cumul",
    adjoint_update_cases_inc =
      "adjoint_cases_inc * if (step%%freq == 0L) 0L else 1L",
    adjoint_update_I = paste("adjoint_N + adjoint_n_IR * p_IR +",
                             "adjoint_p_inf * beta * dt/N + adjoint_I"),
    adjoint_update_R = "adjoint_N + adjoint_R",
    adjoint_update_S = "adjoint_N + adjoint_n_SI * p_SI + adjoint_S",
    adjoint_update_beta = "adjoint_beta + adjoint_p_inf * I * dt/N",
    adjoint_update_gamma =
      "adjoint_gamma + adjoint_p_IR * dt * exp(-gamma * dt)",
    adjoint_update_I0 = "adjoint_I0",
    adjoint_compare_S = "adjoint_S",
    adjoint_compare_I = "adjoint_I",
    adjoint_compare_R = "adjoint_R",
    adjoint_compare_cases_cumul = "adjoint_cases_cumul",
    adjoint_compare_cases_inc =
      "adjoint_cases_inc + (cases_observed/cases_inc - 1L)",
    adjoint_compare_beta = "adjoint_beta",
    adjoint_compare_gamma = "adjoint_gamma",
    adjoint_compare_I0 = "adjoint_I0",
    adjoint_initial_S = "adjoint_S",
    adjoint_initial_I = "adjoint_I",
    adjoint_initial_R = "adjoint_R",
    adjoint_initial_cases_cumul = "adjoint_cases_cumul",
    adjoint_initial_cases_inc = "adjoint_cases_inc",
    adjoint_initial_beta = "adjoint_beta",
    adjoint_initial_gamma = "adjoint_gamma",
    adjoint_initial_I0 = "adjoint_I0 + adjoint_I")
  expect_true(all(names(expected) %in% names(d$equations)))

  for (i in names(expected)) {
    expect_equal(list_to_lang(d$equations[[i]]$rhs$value),
                 parse(text = expected[[i]])[[1]],
                 label = sprintf("Adjoint equation for '%s'", i))
  }
})


test_that("empty differentiate component on models that lack it", {
  ir <- odin_parse({
    initial(x) <- 1
    update(x) <- rnorm(x, 0.1)
    d <- data()
    compare(d) ~ normal(0, scale)
    scale <- user()
  })
  d <- ir_deserialise(ir)
  expect_equal(d$derivative$parameters, character())
  expect_equal(d$derivative$adjoint$variables, character())
  expect_equal(d$derivative$adjoint$components$rhs,
               list(variables = character(), equations = character()))
  expect_equal(d$derivative$adjoint$components$compare,
               list(variables = character(), equations = character()))
  expect_equal(d$derivative$adjoint$components$initial,
               list(variables = character(), equations = character()))
  expect_equal(d$data$adjoint,
               list(length = 0, contents = set_names(list(), character())))
})
