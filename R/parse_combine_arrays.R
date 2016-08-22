## Combine array expressions into a single set of expressions.  This
## means that the pair:
##
##   x[1] <- ...
##   x[2:n] <- ...
##
## will get grouped together as a single x.  All dependencies of the
## expression will be combined, and the source reference also gets
## updated.
##
## This is (potentially) the biggest set of changes that happens to
## the set of equations, and coming out of this function we will
## (potentially) have reordered and resized the eqs, traits and
## names_target elements.
odin_parse_combine_arrays <- function(obj) {
  odin_parse_combine_arrays_check_usage(obj)

  ## TODO: it's possible that this will allow through arrays that are
  ## defined (i.e. have a dim() call) but are never used.  This should
  ## probably be an error.

  ## Now, work through the dim() calls so we establish dimensionality
  ## of arrays.
  nd <- odin_parse_combine_arrays_nd(obj)
  ## And push these back into the dim() calls (which is the names on
  ## the output here).
  eqs <- obj$eqs
  for (i in names(nd)) {
    eqs[[i]]$nd <- nd[[i]]
    eqs[[i]]$lhs$nd <- NULL # cleanup a little, but perhaps leave here?
  }
  obj$eqs <- eqs

  ## Then, work out which sets to combine
  is_array <- obj$traits[, "is_array"]
  if (any(is_array)) {
    i <- match(names(eqs), unique(names(eqs)[is_array]))
    i <- unname(split(which(!is.na(i)), na.omit(i)))
    i_repl <- viapply(i, "[[", 1L)
    eqs[i_repl] <- lapply(i, odin_parse_combine_arrays_1, obj)
    ## Drop the duplicated lines (perhaps)
    i_drop <- unlist(lapply(i, "[", -1L))
    if (length(i_drop) == 0L) {
      obj$eqs <- eqs
    } else {
      obj$eqs <- eqs[-i_drop]
      obj$traits <- obj$traits[-i_drop, , drop=FALSE]
      obj$names_target <- obj$names_target[-i_drop]
    }
  }

  obj
}

## Called for error checking only; no modifications to object:
odin_parse_combine_arrays_check_usage <- function(obj) {
  is_dim <- obj$traits[, "is_dim"]
  is_array <- obj$traits[, "is_array"]
  names_target <- obj$names_target

  ## First, check that every variable that is an array is always
  ## assigned as an array:
  err <- !(is_array | is_dim) & names_target %in% obj$names_target[is_dim]
  if (any(err)) {
    odin_error(sprintf("Array variables must always assign as arrays (%s)",
                       paste(unique(names_target[err]), collapse=", ")),
               get_lines(obj$eqs[err]), get_exprs(obj$eqs[err]))
  }

  ## Then, start checking for duplicates:
  err <- is_duplicated(names(obj$eqs)) & !is_array
  if (any(err)) {
    odin_error(sprintf("Duplicate entries must all be array assignments (%s)",
                       paste(unique(names_target[err]), collapse=", ")),
               get_lines(obj$eqs[err]), get_exprs(obj$eqs[err]))
  }

  ## Prevent:
  ##   x[] <- user()
  ##   x[1] <- 1
  err <- is_duplicated(names(obj$eqs)) & is_array & obj$traits[, "uses_user"]
  if (any(err)) {
    odin_error(sprintf("Duplicate entries may not use user() (%s)",
                       paste(unique(names_target[err]), collapse=", ")),
               get_lines(obj$eqs[err]), get_exprs(obj$eqs[err]))
  }

  name_dim <- vcapply(obj$eqs[is_array], function(x) x$lhs$name_dim)
  err <- which(is_array)[!(name_dim %in% names(obj$eqs))]
  if (length(err) > 0L) {
    odin_error(sprintf("Missing dim() call for %s, assigned as an array",
                       paste(unique(names_target[err]), collapse=", ")),
               get_lines(obj$eqs[err]), get_exprs(obj$eqs[err]))
  }
}

odin_parse_combine_arrays_nd <- function(obj) {
  eqs <- obj$eqs
  is_dim <- obj$traits[, "is_dim"]

  nd <- viapply(eqs[is_dim], function(x) x$lhs$nd)
  nd_user <- nd == 0L
  if (any(nd_user)) {
    ## Then, check for user-driven array sizes; we'll pull nd from the
    ## definition of the variable; it should come in a pair like:
    ##   x[] <- user()
    ##   dim(x) <- user()
    ## and we're after nd from this.
    i <- match(obj$names_target[is_dim][nd_user], names(eqs))

    err <- !obj$traits[i, "uses_user"]
    if (any(err)) {
      odin_error("user-specified dim() must be used with user-specified array",
                 get_lines(eqs[i][err]), get_exprs(eqs[i][err]))
    }

    err <- !obj$traits[i, "is_array"]
    if (any(err)) {
      odin_error("user-specified dim() must be used with array",
                 get_lines(eqs[i][err]), get_exprs(eqs[i][err]))
    }

    nd[nd_user] <- viapply(eqs[i], function(x) x$lhs$nd)
  }

  ## Now we have the nd, some checks to make sure it makes sense.
  for (x in eqs[obj$traits[, "is_array"]]) {
    nd_x <- nd[[x$lhs$name_dim]]
    if (x$lhs$nd != nd_x) {
      odin_error(
        sprintf("Array dimensionality is not consistent (expected %d %s)",
                nd_x, ngettext(nd_x, "index", "indices")),
        x$line, x$expr)
    }
  }

  nd
}

odin_parse_combine_arrays_1 <- function(idx, obj) {
  eqs <- obj$eqs
  x <- eqs[[idx[[1L]]]]

  x$depends <- join_deps(lapply(eqs[idx], function(x) x[["depends"]]))
  x$expr <- lapply(eqs[idx], "[[", "expr")
  x$line <- viapply(eqs[idx], "[[", "line")

  ## TODO: some of the lhs depends stuff will not matter so much now.
  x$lhs$index <- lapply(eqs[idx], function(x) x[["lhs"]][["index"]])
  x$lhs$depends <-
    join_deps(lapply(eqs[idx], function(x) x[["lhs"]][["depends"]]))

  x$rhs$type <- vcapply(eqs[idx], function(x) x[["rhs"]][["type"]])
  x$rhs$depends <-
    join_deps(lapply(eqs[idx], function(x) x[["rhs"]][["depends"]]))
  x$rhs$value <- lapply(eqs[idx], function(x) x[["rhs"]][["value"]])

  ## TODO: All these sanity checks need major overhauls, I think;
  ## these are just here to make sure that we hit all the

  ## Sanity check (needs overhaul I think).  The stopifnot ones guard
  ## against implementation details changing.
  ok <- c("name", "lhs", "rhs", "depends", "expr", "line")
  stopifnot(length(setdiff(unlist(lapply(eqs[idx], names)), ok)) == 0L)
  ## NOTE: mixed type specials are dealt with elsewhere.  By this I
  ## mean that a variable is more than one of initial(), deriv(),
  ## output() and plain.
  used_lhs <- unlist(lapply(eqs[idx], function(x) names(x$lhs)))
  ok <- c("type", "name", "name_target", "index", "nd", "name_dim",
          "depends", "special")
  stopifnot(length(setdiff(used_lhs, ok)) == 0L)

  ## NOTE: These *could* have been done with traits, but are not.  I
  ## think this way around makes sense.
  used_rhs <- unlist(lapply(eqs[idx], function(x) names(x$rhs)))
  single <- c("delay", "user", "interpolate")
  if (any(single %in% used_rhs)) {
    if (length(idx) > 1L) {
      f <- paste0(intersect(single, used_rhs), "()", collapse=", ")
      odin_error(sprintf(
        "%s may only be used on a single-line array assignment", f),
        get_lines(eqs[idx]), get_exprs(eqs[idx]))
    }
  } else {
    ok <- c("type", "depends", "value", "user", "default", "sum")
    stopifnot(length(setdiff(used_rhs, ok)) == 0L)
  }

  ## Array delay variables need to delay on the dimensions of their
  ## "present" array, so that the order of initialisation is always
  ## correct.  In practice I don't think this is a big deal because
  ## array sizing is not time dependent.  However, this resolves a
  ## difficulty in determining the total array size of the delay
  ## bookkeeping indices.
  if (isTRUE(x$rhs$delay)) {
    d <- setdiff(x$rhs$depends_delay$variables, c(obj$vars, INDEX))
    extra <- names(which(obj$traits[d, "is_array"]))
    if (length(extra) > 0L) {
      x$rhs$depends$variables <- union(x$rhs$depends$variables,
                                       array_dim_name(extra))
    }
  }

  x
}
