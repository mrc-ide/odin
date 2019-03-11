generate_c <- function(dat, opts) {
  features_supported <- c("initial_time_dependent", "has_user", "has_output",
                          "discrete", "has_array", "has_stochastic",
                          "has_delay", "has_include", "has_interpolate")
  generate_check_features(features_supported, dat)

  base <- dat$config$base
  dat$meta$c <- list(
    ptr = sprintf("%s_p", dat$meta$internal),
    internal_ds = sprintf("%s_ds", dat$meta$internal),
    internal_t = sprintf("%s_internal", base),
    finalise = sprintf("%s_finalise", base),
    create = sprintf("%s_create", base),
    contents = sprintf("%s_contents", base),
    get_internal = sprintf("%s_get_internal", base),
    set_user = sprintf("%s_set_user", base),
    set_initial = sprintf("%s_set_initial", base),
    use_dde = sprintf("%s_use_dde", base),
    initial_conditions = sprintf("%s_initial_conditions", base),
    metadata = sprintf("%s_metadata", base),
    rhs = sprintf("%s_rhs", base),
    rhs_desolve = sprintf("%s_rhs_desolve", base),
    rhs_dde = sprintf("%s_rhs_dde", base),
    rhs_r = sprintf("%s_rhs_r", base),
    output_dde = sprintf("%s_output_dde", base),
    initmod_desolve = sprintf("%s_initmod_desolve", base))

  if (dat$features$has_delay) {
    dat$data$elements[[dat$meta$c$use_dde]] <-
      list(name = dat$meta$c$use_dde,
           location = "internal",
           storage_type = "bool",
           rank = 0L,
           dimnames = NULL)
  }

  rewrite <- function(x) {
    generate_c_sexp(x, dat$data, dat$meta, names(dat$config$include))
  }
  eqs <- generate_c_equations(dat, rewrite)

  core <- generate_c_compiled(eqs, dat, rewrite)

  lib <- generate_c_compiled_library(dat)
  include <- generate_c_compiled_include(dat)

  if (dat$features$has_delay && dat$features$discrete) {
    ring <- odin_ring_support(FALSE)
  } else {
    ring <- NULL
  }

  if (dat$features$has_interpolate) {
    interpolate <- odin_interpolate_support(FALSE)
  } else {
    interpolate <- NULL
  }

  decl <- c(generate_c_compiled_headers(dat),
            ring$declarations,
            interpolate$declarations,
            generate_c_compiled_struct(dat),
            unname(vcapply(core, "[[", "declaration")),
            lib$declaration,
            include$declaration)
  defn <- c(c_flatten_eqs(c(lapply(core, "[[", "definition"))),
            lib$definition,
            ring$definitions,
            interpolate$definitions,
            include$definition)

  code <- c(decl, defn)

  path <- tempfile(fileext = ".c")
  writeLines(code, path)
  dll <- compile(path, verbose = opts$verbose,
                 compiler_warnings = opts$compiler_warnings)
  dyn_load(dll$dll)

  core_r <- lapply(core, "[[", "name")

  generate_c_class(core_r, dll$base, dat)
}


c_variable_reference <- function(x, data_info, state, rewrite) {
  if (data_info$rank == 0L) {
    sprintf("%s[%s]", state, rewrite(x$offset))
  } else {
    sprintf("%s + %s", state, rewrite(x$offset))
  }
}


c_flatten_eqs <- function(eqs) {
  unlist(unname(eqs))
}


odin_interpolate_support <- function(package) {
  if (package) {
    stop("writeme")
  }
  r_h <- system.file("include/cinterpolate/cinterpolate.h",
                     package = "cinterpolate", mustWork = TRUE)
  r_c <- system.file("include/cinterpolate/cinterpolate.c",
                     package = "cinterpolate", mustWork = TRUE)
  decl <- sprintf('#include "%s"', r_h)
  defn <- sprintf('#include "%s"', r_c)
  list(declarations = decl, definitions = defn)
}
