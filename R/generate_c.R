generate_c_meta <- function(base, internal) {
  list(
    ptr = sprintf("%s_p", internal),
    internal_ds = sprintf("%s_%s_ds", base, internal),
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
}


generate_c_code <- function(dat, options, package) {
  dat$meta$c <- generate_c_meta(dat$config$base, dat$meta$internal)

  if (dat$features$has_delay) {
    dat$data$elements[[dat$meta$c$use_dde]] <-
      list(name = dat$meta$c$use_dde,
           location = "internal",
           storage_type = "bool",
           rank = 0L,
           dimnames = NULL)
  }

  rewrite <- function(x) {
    generate_c_sexp(x, dat$data, dat$meta, dat$config$include$names)
  }

  eqs <- generate_c_equations(dat, rewrite)
  headers <- generate_c_compiled_headers()
  struct <- generate_c_compiled_struct(dat)
  core <- generate_c_compiled(eqs, dat, rewrite)

  is_package <- !is.null(package)
  lib <- generate_c_compiled_library(dat, is_package)
  include <- generate_c_compiled_include(dat)

  if (dat$features$has_delay && dat$features$discrete) {
    ring <- generate_c_support_ring(is_package)
  } else {
    ring <- NULL
  }

  if (dat$features$has_interpolate) {
    interpolate <- generate_c_support_interpolate(is_package)
  } else {
    interpolate <- NULL
  }

  if (is.null(package)) {
    decl <- c(headers,
              ring$declarations,
              interpolate$declarations,
              struct,
              core$declaration,
              lib$declaration,
              unname(include$declarations))
    defn <- c(core$definition,
              lib$definition,
              ring$definitions,
              interpolate$definitions,
              unname(include$definitions))
    list(code = c(decl, defn), core = core$name)
  } else {
    list(headers = headers,
         struct = struct,
         code = core,
         core = core$name,
         lib = lib,
         include = include,
         ring = ring,
         interpolate = interpolate)
  }
}
