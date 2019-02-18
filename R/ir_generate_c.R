generate_c <- function(dat, verbose = FALSE) {
  features_supported <- c("initial_time_dependent", "has_user")
  generate_check_features(features_supported, dat)

  rewrite <- function(x) {
    generate_c_sexp(x, dat$data, dat$meta)
  }
  eqs <- generate_c_equations(dat, rewrite)

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
    initial_conditions = sprintf("%s_initial_conditions", base),
    metadata = sprintf("%s_metadata", base),
    rhs = sprintf("%s_rhs", base),
    rhs_desolve = sprintf("%s_rhs_desolve", base),
    rhs_dde = sprintf("%s_rhs_dde", base),
    rhs_r = sprintf("%s_rhs_r", base),
    initmod_desolve = sprintf("%s_initmod_desolve", base))

  core <- generate_c_compiled(eqs, dat, rewrite)

  decl <- c(generate_c_compiled_headers(dat),
            generate_c_compiled_struct(dat),
            unname(vcapply(core, "[[", "declaration")))
  defn <- c_flatten_eqs(lapply(core, "[[", "definition"))

  if (dat$features$has_user) {
    ## TODO: should filter these?
    lib <- read_user_c(system.file("library2.c", package = "odin"))
    v <- c("get_user_double", "get_user_int", "get_list_element")
    decl <- c(decl, unname(lib$declarations[v]))
    defn <- c(defn, c_flatten_eqs(strsplit(lib$definitions[v], "\n")))
  }

  code <- c(decl, defn)

  path <- tempfile(fileext = ".c")
  writeLines(code, path)
  dll <- compile(path, verbose = verbose)
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
