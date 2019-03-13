generate_c <- function(dat, opts) {
  res <- generate_c_code(dat, opts, NULL)
  code <- res$code
  core <- res$core

  path <- tempfile(fileext = ".c")
  writeLines(code, path)
  dll <- compile(path, verbose = opts$verbose,
                 compiler_warnings = opts$compiler_warnings)
  dyn_load(dll$dll)

  env <- new.env(parent = as.environment("package:base"))
  base <- dat$config$base
  env[[base]] <-
    odin_c_class(base, core, names(dat$user), dat$features, dll$base, dat$ir)
  generate_r_constructor(base, dat$features$discrete, dat$user, env)
}


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


generate_c_code <- function(dat, opts, package) {
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
    generate_c_sexp(x, dat$data, dat$meta, names(dat$config$include))
  }

  eqs <- generate_c_equations(dat, rewrite)
  headers <- generate_c_compiled_headers()
  struct <- generate_c_compiled_struct(dat)
  core <- generate_c_compiled(eqs, dat, rewrite)

  is_package <- !is.null(package)
  lib <- generate_c_compiled_library(dat, is_package)
  include <- generate_c_compiled_include(dat, is_package)

  if (dat$features$has_delay && dat$features$discrete) {
    ring <- generate_c_ring_support(is_package)
  } else {
    ring <- NULL
  }

  if (dat$features$has_interpolate) {
    interpolate <- generate_c_interpolate_support()
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
              include$declaration)
    defn <- c(core$definition,
              lib$definition,
              ring$definitions,
              interpolate$definitions,
              include$definition)
    list(code = c(decl, defn), core = core$name)
  } else {
    r <- generate_c_r(dat, core$name, package)
    list(headers = headers,
         struct = struct,
         core = core,
         lib = lib,
         include = include,
         ring = ring,
         interpolate = interpolate,
         r = r)
  }
}


generate_c_r <- function(dat, core, package) {
  as_str <- function(x, fn = "list") {
    sprintf("%s(%s)", fn, paste(x, collapse = ", "))
  }

  base <- dat$config$base
  class_name <- paste0(".", base)
  user_str <- as_str(dquote(names(dat$user)), "c")

  core_str <- as_str(sprintf(
    '%s = "%s"', names(core), list_to_character(core)))
  features_str <- as_str(sprintf(
    '%s = %s', names(dat$features), vlapply(dat$features, identity)))

  ## TODO:
  ## nicer would be to:
  ##
  ## * format this more nicely
  ## * sort out the ir properly
  ## * not use a triple-colon accessor
  ##
  ## Passing in *just* the path to the ir would probably be nice, but
  ## might have a bit more cost than ideal, but would allow for
  ## creation of a class from an IR object alone.  However, there are
  ## some real gotchas for doing this during package installation I
  ## believe, because we can end up storing the incorrect path if we
  ## evaluate during package installation.
  ret <- collector()
  ret$add('%s <- odin:::odin_c_class("%s", %s, %s, %s, "%s", "%s")',
          class_name, base, core_str, user_str, features_str, package, dat$ir)

  ctor <- generate_r_constructor(class_name, dat$features$discrete, dat$user,
                                 .GlobalEnv)
  ctor <- sub("\\s+$", "", deparse(ctor))
  ctor[[1]] <- sprintf("%s <- %s", base, ctor[[1]])
  ret$add(ctor)

  ret$get()
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
