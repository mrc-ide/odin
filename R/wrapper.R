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
         # "r" = generate_r(dat, options),
         "c" = odin_c_wrapper(ir, options),
         stop(sprintf("Unknown target '%s'", options$target)))
}


odin_c_wrapper <- function(ir, options) {
  dat <- ir_deserialise(ir)
  res <- generate_c_code(dat, options, NULL)
  hash <- hash_string(dat$ir)
  code <- res$code

  data <- list(name = dat$config$base,
               package = paste0(dat$config$base, short_hash(hash)),
               c = list(metadata = res$core$metadata,
                        create = res$core$create,
                        set_user = res$core$set_user,
                        initial = res$core$initial_conditions,
                        rhs_r = res$core$rhs_r,
                        contents = res$core$contents,
                        rhs_dde = res$core$rhs_dde,
                        rhs_desolve = res$core$rhs_desolve,
                        output_dde = "NULL",
                        initmod_desolve = res$core$initmod_desolve))

  if (dat$features$has_output) {
    data$c$output_dde <- dquote(res$core$output)
  }


  dest <- options$workdir
  dir.create(dest, FALSE, TRUE)
  dir.create(file.path(dest, "R"), FALSE, TRUE)
  dir.create(file.path(dest, "src"), FALSE, TRUE)
  dir.create(file.path(dest, "inst/odin"), FALSE, TRUE)

  substitute_template(data, odin_file("template/DESCRIPTION"),
                      file.path(dest, "DESCRIPTION"))
  substitute_template(data, odin_file("template/NAMESPACE"),
                      file.path(dest, "NAMESPACE"))
  substitute_template(data, odin_file("template/ode_c.R"),
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
wrapper_run_basic <- function(t, y, ptr, package, use_dde,
                              rhs_dde, output_dde,
                              rhs_desolve, initmod_desolve,
                              n_out, tcrit, ...) {
  if (use_dde) {
    ## TODO: Because we might invert or otherwise strip the data
    ## returned here, we should use dde's support for naming, and
    ## check that is actually good enough to do this! (this is out
    ## of scope for the immediate work as it's broken in the
    ## current interface).
    ##
    ## NOTE: it's not obvious how (or if!) we can call into use
    ## the native symbols as found by package registration but it
    ## appears not.
    ret <- dde::dopri(y, t, rhs_dde, ptr,
                      dllname = package, parms_are_real = FALSE,
                      n_out = n_out, output = output_dde,
                      ynames = FALSE, tcrit = tcrit, ...)
  } else {
    ## TODO: if this is a delay function we need to use dde, and
    ## in both cases we need to inject the history length.
    ret <- deSolve::ode(y, t, rhs_desolve, ptr,
                        initfunc = initmod_desolve,
                        nout = n_out, dllname = package,
                        tcrit = tcrit, ...)
  }

  ret
}
