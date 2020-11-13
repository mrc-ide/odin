odin2 <- function(x, verbose = NULL, target = NULL, workdir = NULL,
                 validate = NULL, pretty = NULL, skip_cache = NULL,
                 compiler_warnings = NULL, no_check_unused_equations = NULL,
                 no_check_naked_index = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin2_(xx, verbose, target, workdir, validate, pretty, skip_cache,
         compiler_warnings, no_check_unused_equations, no_check_naked_index)
}


odin2_ <- function(x, verbose = NULL, target = NULL, workdir = NULL,
                  validate = NULL, pretty = NULL, skip_cache = NULL,
                  compiler_warnings = NULL, no_check_unused_equations = NULL,
                  no_check_naked_index = NULL) {
  options <- odin_options(verbose = verbose,
                          target = target,
                          workdir = workdir,
                          validate = validate,
                          pretty = pretty,
                          skip_cache = skip_cache,
                          no_check_unused_equations = no_check_unused_equations,
                          no_check_naked_index = no_check_naked_index,
                          compiler_warnings = compiler_warnings)

  ir <- odin_parse_(x, options)
  odin2_generate(ir, options)
}


odin2_generate <- function(ir, options) {
  odin_message(paste("Generating model in", options$target), options$verbose)
  switch(options$target,
         "r" = odin_r_wrapper(ir, options),
         "c" = odin_c_wrapper(ir, options),
         stop(sprintf("Unknown target '%s'", options$target)))
}


## Leave this one largely be for now?
odin_r_wrapper <- function(ir, options) {
  dat <- ir_deserialise(ir)
  generate_r(dat, options)
}


odin_c_wrapper <- function(ir, options) {
  dat <- ir_deserialise(ir)
  res <- generate_c_code(dat, options, NULL)
  hash <- hash_string(dat$ir)
  code <- res$code

  data <- list(name = dat$config$base,
               package = paste0(dat$config$base, short_hash(hash)),
               time = dat$meta$time,
               rhs = if (dat$features$discrete) "update" else "deriv",
               c = list(metadata = res$core$metadata,
                        create = res$core$create,
                        set_user = res$core$set_user,
                        initial = res$core$initial_conditions,
                        rhs_r = res$core$rhs_r,
                        contents = res$core$contents,
                        set_initial = res$core$set_initial))

  if (dat$features$discrete) {
    data$run <- "wrapper_run_discrete"
  } else if (dat$features$has_delay) {
    data$run <- "wrapper_run_delay"
  } else {
    data$run <- "wrapper_run_ode"
  }

  ## Collect up all the C functions, used as pointers.
  if (dat$features$discrete) {
    cfuns <- list(
      rhs_dde = list(name = res$core$rhs_dde))
  } else {
    cfuns <- list(
      rhs_dde = list(name = res$core$rhs_dde),
      rhs_desolve = list(name = res$core$rhs_desolve),
      initmod_desolve = list(name = res$core$initmod_desolve))
  }
  if (dat$features$has_output) {
    cfuns <- c(cfuns, list(output = list(name = res$core$output)))
  }

  ## TODO: It's very likely that we will want to generate a set of
  ## arguments to go with this? Just need to know the correct number
  ## and can do that, though getting types matching is much harder
  ## given the initmod one.
  cfuns_nms <- vcapply(cfuns, "[[", "name", USE.NAMES = FALSE)
  data$registration <- paste(
    sprintf('.C("%s", package = "%s")', cfuns_nms, data$package),
    collapse = "\n      ")

  ## Then the assignment block:
  data$cfuns <- sprintf(
    "list(\n%s)",
    paste(sprintf("      %s = %s", names(cfuns), dquote(cfuns_nms)),
          collapse = ",\n"))

  dest <- options$workdir
  dir.create(dest, FALSE, TRUE)
  dir.create(file.path(dest, "R"), FALSE, TRUE)
  dir.create(file.path(dest, "src"), FALSE, TRUE)
  dir.create(file.path(dest, "inst/odin"), FALSE, TRUE)

  substitute_template(data, odin_file("template/DESCRIPTION"),
                      file.path(dest, "DESCRIPTION"))
  substitute_template(data, odin_file("template/NAMESPACE"),
                      file.path(dest, "NAMESPACE"))
  substitute_template(data, odin_file("template/odin_c.R"),
                      file.path(dest, "R/odin.R"))
  writeLines(code, file.path(dest, "src/odin.c"))
  writeLines(ir, file.path(dest, sprintf("inst/odin/%s.json", data$name)))

  path_reg <- file.path(dest, "src/registration.c")
  tools::package_native_routine_registration_skeleton(dest, path_reg)

  quiet <- !options$verbose

  ## This will do us a fully consistent build/load that we can use for
  ## both the packaging version and the locally built version. Ideally
  ## we could skip over the compile_dll step, but this has already had
  ## the compilation flags bit worked out.
  dll <- compile_dll(dest, compile_attributes = FALSE, quiet = quiet)
  env <- pkgload::load_all(dest, compile = FALSE, recompile = FALSE,
                           warn_conflicts = FALSE, export_all = FALSE,
                           helpers = FALSE, attach_testthat = FALSE,
                           quiet = quiet)

  env$env[[data$name]]
}


## This is a workaround for pkgbuild wanting to build
## debug/unoptimised dlls by default, unless the user has provided a
## Makevars
has_user_makevars <- function() {
  length(environment(pkgbuild::compile_dll)$makevars_user()) > 0
}


compile_dll <- function(...) {
  if (has_user_makevars()) {
    pkgbuild::compile_dll(...)
  } else {
    makevars <- tempfile()
    file.create(makevars)
    on.exit(unlink(makevars))
    withr::with_envvar(
      c("R_MAKEVARS_USER" = makevars),
      pkgbuild::compile_dll(...))
  }
}


substitute_template <- function(data, src, dest) {
  template <- read_lines(src)
  txt <- glue_whisker(template, data)
  writeLines(txt, dest)
}


## We will use a few different sorts of run functions here, based on
## the overall type of model.
wrapper_run_ode <- function(self, private, t, y = NULL, ...,
                            use_names = TRUE, tcrit = NULL) {
  t <- as.numeric(t)
  if (is.null(y)) {
    y <- self$initial(t)
  } else {
    y <- as.numeric(t)
  }

  tcrit <- support_check_interpolate_t(t, private$interpolate_t, tcrit)
  if (private$use_dde) {
    ## TODO: Because we might invert or otherwise strip the data
    ## returned here, we should use dde's support for naming, and
    ## check that is actually good enough to do this! (this is out
    ## of scope for the immediate work as it's broken in the
    ## current interface).
    ret <- dde::dopri(y, t, private$cfuns$rhs_dde, private$ptr,
                      dllname = private$dll, parms_are_real = FALSE,
                      n_out = private$n_out, output = private$cfuns$output_dde,
                      ynames = FALSE, tcrit = tcrit, ...)
  } else {
    ret <- deSolve::ode(y, t, private$cfuns$rhs_desolve, private$ptr,
                        initfunc = private$cfuns$initmod_desolve,
                        nout = private$n_out, dllname = private$dll,
                        tcrit = tcrit, ...)
  }

  ret
}


wrapper_run_delay <- function(self, private, t, y, ...,
                              use_names = TRUE, tcrit = NULL,
                              n_history = DEFAULT_HISTORY_SIZE) {
  if (privateuse_dde) {
    ret <- dde::dopri(y, t, private$cfuns$rhs_dde, private$ptr,
                      dllname = private$dll, parms_are_real = FALSE,
                      n_out = private$n_out, output = private$cfuns$output_dde,
                      ynames = FALSE, tcrit = tcrit,
                      n_history = n_history, ...)
  } else {
    ret <- deSolve::dede(y = y, times = t,
                         func = private$cfuns$rhs_desolve,
                         parms = private$ptr,
                         initfunc = privatae$cfuns$initmod_desolve,
                         nout = private$n_out,
                         dllname = private$dll,
                         tcrit = tcrit,
                         control = list(mxhist = n_history))
  }

  ret
}

wrapper_run_discrete <- function(self, private, step, y = NULL, ...,
                                 use_names = TRUE, replicate = NULL) {
  step <- as_integer(step)
  if (is.null(y)) {
    y <- self$initial(step[[1]])
  }
  support_check_interpolate_t(step, private$interpolate_t, NULL)

  if (is.null(replicate)) {
    ret <- dde::difeq(y, step, private$cfuns$rhs_dde, private$ptr,
                      dllname = private$dll, parms_are_real = FALSE,
                      ynames = FALSE, n_out = private$n_out, ...)
  } else {
    ret <- dde::difeq_replicate(replicate, y, step, private$cfuns$rhs_dde,
                                private$ptr, dllname = private$dll,
                                parms_are_real = FALSE,
                                ynames = FALSE, n_out = private$n_out, ...)
  }

  if (use_names) {
    colnames(ret) <- private$ynames
  } else {
    colnames(ret) <- NULL
  }

  ret
}



##' @export
`[[.odin_generator` <- function(x, i) {
  attr(x, "generator", exact = TRUE)[[i]]
}


##' @export
`$.odin_generator` <- function(x, name) {
  x[[name]]
}
