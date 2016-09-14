odin_parse_dependencies <- function(obj) {
  obj$deps_rec <- odin_parse_dependencies_deps(obj)
  obj$stage <- odin_parse_dependencies_stage(obj)

  ## Set the stage in every element; this might actually be worth
  ## changing to only use the stage vector though; used mostly in
  ## generate.R
  for (i in names(obj$eqs)) {
    obj$eqs[[i]]$stage <- obj$stage[[i]]
  }

  ## Compute the overall stage for computing dimensions:
  dim_stage <- max(c(STAGE_CONSTANT,
                     obj$stage[names_if(obj$traits[, "is_dim"])]))

  if (dim_stage >= STAGE_TIME) {
    err <- obj$stage[obj$traits[, "is_dim"]] == STAGE_TIME
    odin_error("Array extent is determined by time",
               get_lines(obj$eqs[err]), get_exprs(obj$eqs[err]))
  }

  ## NOTE: Equation reordering; *must* update traits and names_target
  ## at the same time.
  ##
  ## NOTE: This keeps the equations topologically sorted, though the
  ## order may vary from deps_rec; the equations are also sorted by
  ## stage.
  i <- match(intersect(names(obj$stage), names(obj$eqs)), names(obj$eqs))
  obj$eqs <- obj$eqs[i]
  obj$traits <- obj$traits[i, ]
  obj$names_target <- obj$names_target[i]
  obj$info$dim_stage <- dim_stage

  ## TODO: The other thing that is needed through here is going to be
  ## information about _exactly_ which variables need unpacking from
  ## the structs; there will be models where time is never
  ## _explicitly_ used (the lorenz attractor is one such model).
  ## There will be models where some variables have derivatives
  ## computed but the value of the variable is never referenced in the
  ## calculations and there will be (in the dde/output case) variables
  ## that don't need unpacking.  I think that if I change the dummy
  ## 't' variable to be '<time>' and use that for detecting stage but
  ## look out for an explicit time variable that would be preferable.

  ## Need to score usage in:
  ##   initialisation
  ##   derivs
  ##   output
  ##   various delays?

  obj
}

odin_parse_dependencies_deps <- function(obj) {
  eqs <- obj$eqs
  nms <- names(eqs)
  vars <- obj$vars

  ## NOTE: Here, we filter the dependency lists to exclude any self
  ## referencing variables.  These are allowed only for array
  ## variables and this is checked in parse_expr.  INDEX variables
  ## don't count, and it's possible for the empty variable to make it
  ## through here (TODO: exclude that at source?)
  deps <- lapply(eqs, function(el)
    setdiff(el$depends$variables, c(el$name, INDEX, "")))

  ## Check for variables that don't exist:
  msg <- lapply(deps, setdiff, c(nms, vars, TIME))
  i <- lengths(msg) > 0L
  if (any(i)) {
    msg <- sort(unique(unlist(msg)))
    ## TODO: this is not *variable* as such.
    fmt <- ngettext(length(msg), "Unknown variable %s",  "Unknown variables %s")
    odin_error(sprintf(fmt, paste(msg, collapse=", ")),
               get_lines(eqs[i]), get_exprs(eqs[i]))
  }

  ## For the derivative calculations the variables come in with no
  ## dependencies because they are provided by the integrator (but
  ## we'll add an implicit time dependency).
  dummy <- c(list(t=character(0)),
             setNames(rep(list(character(0)), length(vars)), vars))
  order <- topological_order(c(deps, dummy))

  ## Then, we work out the recursive dependencies; this is the entire
  ## dependency chain of a thing; including its dependencies, its
  ## dependencies dependencies and so on.
  recursive_dependencies(order, c(deps, dummy), vars)
}

odin_parse_dependencies_stage <- function(obj) {
  deps_rec <- obj$deps_rec
  n <- length(deps_rec)

  ## Then, we can get the stage for each equation.  Start by assuming
  ## that the stage is constant but loop over all the depenencies of
  ## each equation to see if we're picked up a stronger dependency
  ## (passing through in in the topological sort order).
  ##
  ## * Equations involving user() are "user" stage
  ##
  ## * Variables involving a variable, time, a delay equation, or an
  ##   interpolation equation are "time" stage.
  ##
  ## * deriv() and output() equations are "time" stage, but nothing
  ##   depends on them.
  stage <- setNames(rep(STAGE_CONSTANT, n), names(deps_rec))
  stage[names(which(obj$traits[, "uses_user"]))] <- STAGE_USER

  v <- c("is_output", "is_deriv", "uses_delay", "uses_interpolate")
  stage[names(which(apply(obj$traits[, v], 1, any)))] <- STAGE_TIME
  stage[TIME] <- STAGE_TIME
  stage[obj$vars] <- STAGE_TIME
  stage[vlapply(obj$eqs, "[[", "stochastic")] <- STAGE_TIME

  ## In topological order, determine inherited stage (a initial/time stage
  ## anywhere in a chain implies a initial/time stage).
  for (i in seq_len(n)) {
    stage[[i]] <- max(stage[[i]], stage[deps_rec[[i]]])
  }

  ## Adjust the order so that it's by stage first, and then the order.
  ## This should not create any impossible situations because of the
  ## stage treatent above.

  ## NOTE: The final stage vector contains TIME and vars; it must
  ## continue to contain these unless they are filtered from the
  ## *names and contents* of deps_rec.  In that case, we can do:
  ##
  ##    stage[names(stage) %in% names(obj$eqs)]
  ##
  ## after this step to put this in the same order as eqs

  stage[order(stage)]
}
