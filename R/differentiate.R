adjoint_model <- function(parameters, dat) {
  variables <- vcapply(dat$data$variable$contents, "[[", "name")

  adjoint <- list(rhs = adjoint_update(variables, parameters, dat),
                  compare = adjoint_compare(variables, parameters, dat),
                  initial = adjoint_initial(variables, parameters, dat))

  equations <- unlist(lapply(unname(adjoint), function(x) x$equations),
                      FALSE, TRUE)

  ## Why is this not successfully pulling out our initial conditions;
  ## some error in the processing of compare and initial it seems
  components <- lapply(adjoint, function(x) {
    list(variables = c(x$depends$variables, x$depends$adjoint),
         equations = x$order)
  })

  ## Drop equations that are never referenced in any part of the
  ## adjoint model; these are fairly harmless but not interesting:
  eqs_used <- unique(unlist(components, TRUE, FALSE))

  equations <- equations[names(equations) %in% eqs_used]
  data <- adjoint_data(variables, parameters, equations, dat)

  nms_valid <- c(names(equations),
                 names(data$elements),
                 names(dat$data$elements))

  stopifnot(
    !any(duplicated(names(equations))),
    ## Don't clobber any existing equations:
    !any(names(equations) %in% names(dat$equations)),
    ## Don't reference impossible equations:
    all(names(eqs_used) %in% names(equations)),
    ## Everything is known about at this point:
    all(eqs_used %in% nms_valid))

  list(equations = equations,
       data = data,
       variables = adjoint_name(variables),
       components = components)
}


adjoint_update <- function(variables, parameters, dat) {
  ## The update set is really the full set of things; we need all the
  ## equations, including those that are not actually time dependent,
  ## out of the graph, *except* those that are initial conditions or
  ## data comparison functions.
  nms_eqs <- names_if(vlapply(dat$equations, function(x) {
    !(x$type %in% c("compare", "user")) && !identical(x$lhs$special, "initial")
  }))
  prefix <- "update"
  eqs_update_parameters <- set_names(dat$equations[parameters],
                                     sprintf("%s_%s", prefix, parameters))
  eqs <- c(dat$equations[nms_eqs], eqs_update_parameters)

  nms_lhs <- vcapply(eqs, function(x) x$lhs$name_lhs, USE.NAMES = FALSE)
  nms <- names(eqs)
  accumulate <- rep(c(FALSE, TRUE), c(length(nms_eqs), length(parameters)))

  ## We then need to get all dependencies from this set of equations;
  ## this is what the algorithm traverses through.
  deps <- lapply(eqs, function(eq) eq$depends$variables %||% character())
  data_info <- dat$data$elements[nms_lhs]

  res <- Map(adjoint_equation, nms, data_info, accumulate,
             MoreArgs = list("update", deps, eqs, variables, parameters))
  names(res) <- vcapply(res, "[[", "name")

  ## Work out the "stage" of these; we just count stage here as
  ## "time", though we could use "adjoint" perhaps (though we'd just
  ## end up discarding that).
  stage <- c(viapply(dat$data$elements, "[[", "stage"),
             set_names(rep_len(STAGE_TIME, length(res)), names(res)))

  deps_adj <- lapply(res, function(eq) eq$depends$variables %||% character())
  deps_all <- c(deps_adj, deps)
  deps_rec <- recursive_dependencies(names(deps_all), deps_all)

  ## Then we do a prune down to the things that we care about
  is_update <- vlapply(eqs, function(eq) {
    identical(eq$lhs$special, "update") || isTRUE(eq$user$differentiate)
  })
  include <- adjoint_name(names_if(is_update))

  used <- unique(unlist(deps_rec[include], FALSE, FALSE))
  order <- intersect(topological_order(deps_all), union(include, used))
  order <- order[stage[order] == STAGE_TIME]

  ## Yet one more dig into the set of variables to pull this out; this
  ## can surely be simplified.
  used_variables <- intersect(unique(unlist(deps_rec[order], FALSE, FALSE)),
                              variables)
  used_adjoint <- intersect(used, adjoint_name(c(variables, parameters)))

  list(equations = res,
       order = order,
       depends = list(variables = used_variables, adjoint = used_adjoint))
}


adjoint_compare <- function(variables, parameters, dat) {
  eqs <- dat$equations[dat$components$compare$equations]
  deps <- lapply(eqs, function(eq) eq$depends$variables %||% character())
  role <- "compare"

  nms <- c(variables, parameters)
  data_info <- dat$data$elements[nms]
  res <- Map(adjoint_equation, nms, data_info,
             MoreArgs = list(TRUE, role, deps, eqs, variables, parameters))
  names(res) <- vcapply(res, "[[", "name")

  stage <- c(viapply(dat$data$elements, "[[", "stage"),
             set_names(rep_len(STAGE_TIME, length(res)), names(res)))

  deps_adj <- lapply(res, function(eq) eq$depends$variables %||% character())
  deps_all <- c(deps_adj, deps)
  deps_rec <- recursive_dependencies(names(deps_all), deps_all)

  include <- adjoint_name(sprintf("%s_%s", role, c(variables, parameters)))
  used <- unique(unlist(deps_rec[include], FALSE, FALSE))
  order <- intersect(topological_order(deps_all), union(include, used))
  order <- order[stage[order] == STAGE_TIME]

  variables <- vcapply(dat$data$variable$contents, "[[", "name")
  used_variables <- intersect(unique(unlist(deps_rec[order], FALSE, FALSE)),
                              variables)
  used_adjoint <- intersect(used, adjoint_name(c(variables, parameters)))

  list(equations = res,
       order = order,
       variables = intersect(unlist(deps_rec[order], FALSE, FALSE), variables),
       depends = list(variables = used_variables,
                      adjoint = used_adjoint))
}


adjoint_initial <- function(variables, parameters, dat) {
  eqs <- c(
    dat$equations[dat$components$initial$equations],
    dat$equations[grep("^initial_", names(dat$equations), value = TRUE)])
  deps <- lapply(eqs, function(eq) eq$depends$variables)
  role <- "initial"

  nms <- c(variables, parameters)
  data_info <- dat$data$elements[nms]
  res <- Map(adjoint_equation, nms, data_info,
             MoreArgs = list(TRUE, role, deps, eqs, variables, parameters))
  names(res) <- vcapply(res, "[[", "name")

  stage <- c(viapply(dat$data$elements, "[[", "stage"),
             set_names(rep_len(STAGE_TIME, length(res)), names(res)))

  deps_adj <- lapply(res, function(eq) eq$depends$variables %||% character())
  deps_all <- c(deps_adj, deps)
  deps_rec <- recursive_dependencies(names(deps_all), deps_all)

  include <- adjoint_name(sprintf("%s_%s", role, c(variables, parameters)))
  used <- unique(unlist(deps_rec[include], FALSE, FALSE))
  order <- intersect(topological_order(deps_all), union(include, used))
  order <- order[stage[order] == STAGE_TIME]
  used_variables <- intersect(unique(unlist(deps_rec[order], FALSE, FALSE)),
                              variables)
  used_adjoint <- intersect(used, adjoint_name(c(variables, parameters)))

  list(equations = res,
       order = order,
       depends = list(variables = used_variables,
                      adjoint = used_adjoint))
}


adjoint_equation <- function(name, data_info, accumulate, role, deps, eqs,
                             variables, parameters) {
  name_data <- data_info$name
  use <- names(which(vlapply(deps, function(x) name_data %in% x)))
  parts <- lapply(eqs[use], function(eq) {
    if (eq$type == "data") {
      return(1)
    }
    if (eq$type == "compare") {
      ## TODO: some care needed here for interesting args,
      ## unfortunately; try the exponential noise trick inline?
      expr <- log_density(eq$rhs$distribution,
                          eq$lhs$name_data,
                          eq$rhs$args)
      ## This is only correct if the lhs is data, which it should
      ## always be, but we should check this, really, somewhere!
      name_adjoint <- 1
    } else {
      expr <- make_deterministic(eq$rhs$value)
      name_adjoint <- as.name(adjoint_name(eq$lhs$name_data))
    }

    maths$times(name_adjoint, differentiate(expr, name_data))
  })
  if (accumulate) {
    parts <- c(list(as.name(adjoint_name(name_data))), parts)
  }

  name_lhs <- adjoint_name(name_data)
  if (name_data %in% c(variables, parameters)) {
    name_equation <- adjoint_name(sprintf("%s_%s", role, name_data))
  } else {
    name_equation <- name_lhs
  }

  rhs <- list(value = fold_add(parts))
  depends <- find_symbols(rhs$value)
  lhs <- list(
    name_equation = name_equation,
    name_data = name_lhs,
    name_lhs = name_lhs,
    storage_type = "double")

  ## This needs making more flexible later; I see this in the toy
  ## SIR model while looking at adjoint_compare_* for any variable
  ## or parameter.
  stopifnot(data_info$rank == 0)
  type <- "expression_scalar"

  list(name = name_equation,
       type = type,
       source = integer(0),
       depends = depends,
       lhs = lhs,
       rhs = rhs)
}


adjoint_data <- function(variables, parameters, equations, dat) {
  nms_eqs <- unique(vcapply(equations, function(x) x$lhs$name_data))
  nms_vars <- adjoint_name(c(variables, parameters))

  ## This only works while we have no arrays:
  stopifnot(!dat$features$has_array)
  length <- length(nms_vars)
  contents <- Map(list,
                  name = nms_vars,
                  offset = seq_along(nms_vars) - 1L)

  elements <- lapply(nms_eqs, function(nm) {
    location <- if (nm %in% nms_vars) "adjoint" else "transient"
    list(name = nm,
         location = location,
         storage_type = "double",
         rank = 0L,
         dimnames = NULL,
         stage = STAGE_ADJOINT)
  })
  names(elements) <- nms_eqs

  list(adjoint = list(length = length,
                      contents = contents),
       elements = elements)
}


fold_add <- function(x) {
  x <- x[!vlapply(x, is.null)]
  if (length(x) == 0) {
    0
  } else if (length(x) == 1) {
    x[[1]]
  } else {
    ret <- x[[1]]
    for (el in x[-1]) {
      ret <- maths$plus(ret, el)
    }
    ret
  }
}
