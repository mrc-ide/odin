adjoint_update <- function(variables, parameters, dat) {
  ## The update set is really the full set of things; we need all the
  ## equations, including those that are not actually time dependent,
  ## out of the graph, *except* those that are initial conditions or
  ## data comparison functions. We don't specially mark initial
  ## conditions anywhere so we need to use some name matching here,
  ## which is unfortunate (this regular expression appears throughout
  ## this file and is something that we need to update in the schema I
  ## think)
  ##
  ## Parameters are a bit harder because this is what we are flushing
  ## towards, and because we want these at the end. So we keep these
  ## at the end and go with the set that we're asked to report on
  ## only.
  nms_eqs <- names_if(vlapply(dat$equations, function(x) {
    !(x$type %in% c("compare", "user")) && !identical(x$lhs$special, "initial")
  }))
  prefix <- "update"
  eqs_update_parameters <- set_names(dat$equations[parameters],
                                     sprintf("%s_%s", prefix, parameters))
  eqs <- c(dat$equations[nms_eqs], eqs_update_parameters)

  ## for my adjoint eqs, nms_lhs and nms are the wrong way around...

  nms_lhs <- vcapply(eqs, function(x) x$lhs$name_lhs, USE.NAMES = FALSE)
  nms <- names(eqs)
  accumulate <- rep(c(FALSE, TRUE), c(length(nms_eqs), length(parameters)))

  ## We then need to get all dependencies from this set of equations;
  ## this is what the algorithm traverses through.
  deps <- lapply(eqs, function(eq) eq$depends$variables %||% character())
  res <- Map(adjoint_equation, nms, nms_lhs, accumulate,
             MoreArgs = list(deps, eqs))
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

  nms <- c(variables, parameters)
  res <- Map(adjoint_equation, nms, nms, MoreArgs = list(TRUE, deps, eqs))
  names(res) <- sprintf("compare_%s", vcapply(res, "[[", "name"))

  stage <- c(vcapply(dat$data$elements, "[[", "stage"),
             set_names(rep_len("adjoint", length(res)), names(res)))

  deps_adj <- lapply(res, function(eq) eq$depends$variables %||% character())
  deps_all <- c(deps_adj, deps)
  deps_rec <- recursive_dependencies(names(deps_all), deps_all)

  include <- sprintf("compare_%s", name_adjoint(c(variables, parameters)))
  used <- unique(unlist(deps_rec[include], FALSE, FALSE))
  order <- intersect(topological_order(deps_all), union(include, used))
  order <- order[stage[order] %in% c("time", "adjoint")]

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

  nms <- c(variables, parameters)
  res <- Map(adjoint_equation, nms, nms, MoreArgs = list(TRUE, deps, eqs))
  names(res) <- sprintf("initial_%s", vcapply(res, "[[", "name"))

  stage <- c(vcapply(dat$data$elements, "[[", "stage"),
             set_names(rep_len("adjoint", length(res)), names(res)))

  deps_adj <- lapply(res, function(eq) eq$depends$variables %||% character())
  deps_all <- c(deps_adj, deps)
  deps_rec <- recursive_dependencies(names(deps_all), deps_all)

  include <- sprintf("initial_%s", name_adjoint(c(variables, parameters)))
  used <- unique(unlist(deps_rec[include], FALSE, FALSE))
  order <- intersect(topological_order(deps_all), union(include, used))
  order <- order[stage[order] %in% c("time", "adjoint")]
  used_variables <- intersect(unique(unlist(deps_rec[order], FALSE, FALSE)),
                              variables)
  used_adjoint <- intersect(used, name_adjoint(c(variables, parameters)))

  list(equations = res,
       order = order,
       depends = list(variables = used_variables,
                      adjoint = used_adjoint))
}


adjoint_equation <- function(name, name_lhs, accumulate, deps, eqs) {
  ## TODO: also pass the data types here through too, we will need
  ## these soon.
  use <- names(which(vlapply(deps, function(x) name_lhs %in% x)))
  parts <- lapply(eqs[use], function(eq) {
    if (eq$type == "data") {
      return(1)
    }
    if (eq$type == "compare") {
      ## TODO: some care needed here for interesting args,
      ## unfortunately; try the exponential noise trick inline?
      expr <- log_density(eq$compare$distribution, eq$lhs, eq$compare$args)
      ## This is only correct if the lhs is data, which it should
      ## always be, but we should check this, really.
      name_adjoint <- 1
    } else {
      expr <- make_deterministic(eq$rhs$value)
      name_adjoint <- as.name(adjoint_name(eq$lhs$name_data))
    }

    expr_d <- differentiate(expr, name_lhs)
    if (is.numeric(expr_d) && expr_d == 0) {
      NULL
    } else {
      call("*", name_adjoint, differentiate(expr, name_lhs))
    }
  })
  if (accumulate) {
    parts <- c(list(as.name(adjoint_name(name_lhs))), parts)
  }
  rhs_expr <- fold_add(parts)
  rhs <- list(value = rhs_expr)

  list(name = adjoint_name(name),
       type = "expression_scalar", # can get from parent?
       depends = find_symbols(rhs_expr),
       lhs = adjoint_name(name_lhs),
       rhs = rhs)
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


## Full set of densities to support come from dust, which is a bit
## weird.
log_density <- function(distribution, target, args) {
  switch(
    distribution,
    poisson = substitute(
      lambda * log(x) - x - lfactorial(lambda),
      list(x = as.name(args[[1]]), lambda = as.name(target))),
    stop(sprintf("Unsupported distribution '%s'", distribution)))
}


deterministic_rules <- list(
  unif_rand = function(expr) {
    0.5
  },
  norm_rand = function(expr) {
    0
  },
  exp_rand = function(expr) {
    1
  },
  rbeta = function(expr) {
    substitute(a / (a + b), list(a = expr[[2]], b = expr[[3]]))
  },
  rbinom = function(expr) {
    substitute(n * p, list(n = expr[[2]], p = expr[[3]]))
  },
  rcauchy = function(expr) {
    ## This needs to flow through to line numbers eventually, or we
    ## need to throw an error if it remains in the code (so allow it
    ## only if it is never used)
    stop("The cauchy distribution has no mean, and may not be used")
  },
  rchisq = function(expr) {
    expr[[2]]
  },
  rexp = function(expr) {
    substitite(1 / rate, list(rate = expr[[2]]))
  },
  rf = function(expr) {
    ## TODO: only valid for df2 > 2!
    substitite(df2 / (df2 - 2), list(df2 = expr[[3]]))
  },
  rgamma = function(expr) {
    substitute(shape / rate, list(shape = expr[[2]], rate = expr[[3]]))
  },
  rgeom = function(expr) {
    substitute(1 / p, list(p = expr[[2]]))
  },
  rhyper = function(expr) {
    substitute(k * m / (m + n),
               list(m = expr[[2]], n = expr[[3]], k = expr[[4]]))
  },
  rlogis = function(expr) {
    expr[[2]]
  },
  rlnorm = function(expr) {
    substitute(exp(mu + sigma^2 / 2), list(mu = expr[[2]], sigma = expr[[3]]))
  },
  rnbinom = function(expr) {
    substitute(n * p * (1 - p), list(n = expr[[2]], p = expr[[3]]))
  },
  rnorm = function(expr) {
    expr[[2]]
  },
  rpois = function(expr) {
    expr[[2]]
  },
  rt = function(expr) {
    ## only if df > 1
    0
  },
  runif = function(expr) {
    substitute((a + b) / 2, list(a = expr[[2]], b = expr[[3]]))
  },
  rweibull = function(expr) {
    substitute(b * gamma(1 + 1 / a), list(a = expr[[2]], b = expr[[3]]))
  },
  rwilcox = function(expr) {
    substitite(m * n / 2, list(m = expr[[2]], n = expr[[3]]))
  },
  rsignrank = function(expr) {
    substitite(n * (n + 1) / 4, list(n = expr[[2]]))
  })


make_deterministic <- function(expr) {
  if (is.recursive(expr) && is.symbol(expr[[1]])) {
    fn <- as.character(expr[[1]])
    if (fn %in% names(deterministic_rules)) {
      expr <- deterministic_rules[[fn]](expr)
    }
  }
  if (is.recursive(expr)) {
    expr <- as.call(lapply(expr, make_deterministic))
  }
  expr
}
