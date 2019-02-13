## When it comes time to simplify the tangle of preprocessing code
## below the two ways forward might include rewriting this to use only
## the equations and not things like "traits".  Then we can reorder at
## will.  If we can impose our own order at some point by resorting
## we'd be good to go.
##
## * traits
## * stage
## * deps_rec
##
## * output_info & variable_info are probably ok
## * info$has_* are ok
## * config probably ok

odin_build_ir <- function(x, type = NULL, validate = FALSE, pretty = TRUE) {
  xp <- odin_preprocess(x)
  dat <- ir_prep(odin_parse(xp))

  ## The core functions that we're looking to create are:
  ##
  ##   - initial
  ##   - deriv (or rhs, more generally)
  ##   - output
  ##
  ## for each of these we should declare what we need by recursing
  ## through the dependency tree.  This belongs in the ir and can be
  ## represented simply as a set of arrays
  ir_dat <- list(config = ir_config(dat),
                 meta = ir_meta(dat),
                 features = ir_features(dat),
                 data = ir_data(dat),
                 equations = ir_equations(dat),
                 components = ir_components(dat),
                 user = ir_user(dat),
                 interpolate = ir_interpolate(dat),
                 source = vcapply(xp$exprs, deparse_str))
  ir <- ir_serialise(ir_dat, pretty)
  if (validate) {
    ir_validate(ir, TRUE)
  }
  ir
}


## This is going to do some of the prep work that should have been
## done in the parse and which will move there once this settles down
## (in fact, once the ir version is working, we'll refactor the parse
## code entirely to more directly produce the ir).
ir_prep <- function(dat) {
  ## Add extra equations
  dat <- ir_prep_offset(dat, FALSE)
  if (dat$info$has_output) {
    dat <- ir_prep_offset(dat, TRUE)
  }

  if (dat$info$has_array) {
    dat <- ir_prep_dim(dat)
  }
  dat <- ir_prep_delay(dat)

  location <- set_names(rep("internal", nrow(dat$traits)),
                        rownames(dat$traits))
  location[dat$traits[, "is_deriv"]] <- "variable"
  location[dat$traits[, "is_output"]] <- "output"

  ## initial: this is going to be initial conditions and their
  ## non-constant dependencies
  vars <- dat$variable_info$order

  if (dat$info$has_interpolate) {
    i <- dat$traits[, "uses_interpolate"]
    tmp <- lapply(dat$eqs[i], ir_prep_interpolate, dat)
    ## Now we need to patch this into the data.  This is not much fun
    ## because we change orders of things!

    ## TODO: when the parse code is refactored, look carefully at this
    ## as the allocation and the use go into such different parts of
    ## the graph that the current approach is pretty annoying.
    ## However, where the original bits are put is just fine for now.
    ## We'll organise the allocations into the right place too.
    ##
    ## Interpolation is a bit weird because the allocation never ends
    ## up in a dependency and is always going to be done in the user
    ## stage at the end.  So the *use* ends up as a dependency in
    ## others but has no actual dependencies (save time) and the
    ## allocation depends on others.  As such the ordering is always
    ## safe to be: allocation as the first element before time, use as
    ## the first element after time.
    dat$eqs[i] <- lapply(tmp, "[[", "use")
    alloc <- set_names(lapply(tmp, "[[", "alloc"),
                       vcapply(tmp, function(x) x$alloc$name))
    dat$eqs <- c(dat$eqs, alloc)
    dat$stage <- c(dat$stage, viapply(alloc, "[[", "stage"))
    location <- c(location, set_names(rep("internal", sum(i)), names(alloc)))
    tmp <- dat$traits[i, , drop = FALSE]
    rownames(tmp) <- names(alloc)
    dat$traits <- rbind(dat$traits, tmp)
  }

  dat$eqs <- lapply(dat$eqs, ir_prep_sum)

  ## rhs:
  ## * all equations with a time step?
  ## * all time-dependent dependencies of derivatives? [using this one]
  v <- names_if(dat$traits[, "is_deriv"])
  v_dep <- unique(unlist(dat$deps_rec[v], use.names = FALSE))
  eq_eval_rhs <- set_names(
    names(dat$eqs) %in% c(v, v_dep[dat$stage[v_dep] == STAGE_TIME]),
    names(dat$eqs))
  var_used_rhs <- set_names(vars %in% v_dep, vars)

  ## output:
  v <- names_if(dat$traits[, "is_output"])
  v_dep <- unique(unlist(dat$deps_rec[v], use.names = FALSE))
  eq_eval_output <- set_names(
    names(dat$eqs) %in% c(v, v_dep[dat$stage[v_dep] == STAGE_TIME]),
    names(dat$eqs))
  var_used_output <- set_names(vars %in% v_dep, vars)

  ## create:
  ## * all constant variables
  eq_eval_create <- dat$stage[names(dat$eqs)] == STAGE_CONSTANT

  ## initial:
  v <- names_if(dat$traits[, "is_initial"])
  v_dep <- unique(c(v, unlist(dat$deps_rec[v], use.names = FALSE)))
  eq_eval_initial <- set_names(
    names(dat$eqs) %in% v_dep[dat$stage[v_dep] == STAGE_TIME],
    names(dat$eqs))

  ## user:
  v <- names_if(
    dat$stage[names(dat$eqs)] == STAGE_USER &
    vcapply(dat$eqs, function(eq) eq$lhs$type) != "null")
  eq_eval_user <- set_names(names(dat$eqs) %in% v, names(dat$eqs))

  for (i in seq_along(dat$eqs)) {
    dat$eqs[[i]]$lhs$location <- location[[dat$eqs[[i]]$name]]
    dat$eqs[[i]]$deps_rec <- dat$deps_rec[[i]] %||% character(0)
  }

  dat$evaluated <- list(rhs = names_if(eq_eval_rhs),
                        output = names_if(eq_eval_output),
                        create = names_if(eq_eval_create),
                        user = names_if(eq_eval_user),
                        initial = names_if(eq_eval_initial))
  dat$variable_info$used <- list(rhs = names_if(var_used_rhs),
                                 output = names_if(var_used_output))

  dat
}


ir_prep_offset <- function(dat, output) {
  info <- if (output) dat$output_info else dat$variable_info
  i <- vlapply(info$offset, is.language)
  if (any(i)) {
    ## Here, add auxillary equations into the right place - eventually
    ## we'll do this directly from odin's parse!  Things that need
    ## modifying:
    ##
    ## - traits
    ## - eqs
    ## - stage
    ## - deps_rec
    ##
    ## base the offsets off of the dimension of the array being worked
    ## with in terms of position in the graph, but make sure that the
    ## stage reflects their dependenies and not the target.
    ##
    ## We'll base these off of the equation that defines the variable
    ## I think, because that's the equation that requires the
    ## existance of the offset.

    deps_rec <- dat$deps_rec
    stage <- dat$stage

    eqs <- vector("list", sum(i))

    for (j in seq_along(eqs)) {
      eq <- ir_prep_offset1(which(i)[[j]], info, dat)
      deps_rec[[eq$name]] <- sort(unique(c(
        eq$depends$variables,
        unlist(deps_rec[eq$depends$variables], FALSE, FALSE))))
      eq$stage <- max(stage[eq$depends$variables])
      stage[[eq$name]] <- eq$stage
      eqs[[j]] <- eq
    }
    names(eqs) <- vcapply(eqs, "[[", "name")

    traits <- dat$traits[seq_along(eqs), , drop = FALSE]
    traits[] <- FALSE
    traits[, "is_symbol"] <- TRUE
    rownames(traits) <- names(eqs)

    dat$eqs <- c(dat$eqs, eqs)
    dat$stage <- stage
    dat$deps_rec <- deps_rec
    dat$traits <- rbind(dat$traits, traits)

    if (!output) {
      after <- match(array_dim_name(sub("^offset_", "", names(eqs))),
                     names(dat$eqs))
      stopifnot(!any(is.na(after)))
      ## This is not strictly correct and would be better dealt with
      ## by doing the topological sort differently with these
      ## auxiliary equations included properly.  But that can wait for
      ## the rewrite.
      j <- ir_prep_interleave_after(after, length(dat$eqs))
      stopifnot(identical(names(dat$eqs), rownames(dat$traits)))
      dat$eqs <- dat$eqs[j]
      dat$traits <- dat$traits[j, , drop = FALSE]
    }
  }
  dat
}


ir_prep_sum <- function(eq) {
  rewrite_sum <- function(x) {
    if (is.recursive(x)) {
      fn <- as.character(x[[1L]])
      if (fn %in% FUNCTIONS_SUM) {
        x <- as.call(c(list(quote(odin_sum)), as.list(x)[-1L]))
      } else {
        x[-1L] <- lapply(x[-1L], rewrite_sum)
      }
    }
    x
  }

  if (any(FUNCTIONS_SUM %in% eq$depends$functions)) {
    if (eq$lhs$type == "array") {
      eq$rhs$value <- lapply(eq$rhs$value, rewrite_sum)
    } else {
      eq$rhs$value <- rewrite_sum(eq$rhs$value)
    }
    eq$depends$functions <- c(setdiff(eq$depends$functions, FUNCTIONS_SUM),
                              "odin_sum")
  }
  eq
}


ir_prep_offset1 <- function(i, info, dat) {
  nm <- names(info$offset)[[i]]
  value <- info$offset[[i]]
  depends <- find_symbols(value)
  parent <- dat$eqs[[info$names[[i]]]]
  stopifnot(!is.null(parent))
  list(name = nm,
       lhs = list(type = "symbol",
                  name = nm,
                  data_type = "int"),
       rhs = list(type = "expression",
                  value = value,
                  depends = depends),
       depends = depends,
       expr = parent$expr,
       expr_str = parent$expr_str,
       line = parent$line)
}


ir_prep_interleave_index <- function(i, m) {
  stopifnot(sum(i) == length(m))
  j <- c(seq_along(i), rep(unname(which(i)), m))
  j[which(i)] <- NA
  order(j, na.last = FALSE)[-seq_len(sum(i))]
}


ir_prep_interleave_after <- function(after, len) {
  x <- seq_along(after) + len - length(after)
  i <- seq_len(max(x))
  i[x] <- after + 0.5
  order(i)
}


ir_prep_dim_user <- function(nm, dat) {
  ## if we run this lot in set user then I think that it will work -
  ## it needs to really for this to all make any sense!  So let's give
  ## it look.  We will rewrite:
  eq <- dat$eqs[[nm]]
  eq <- list(
    name = nm,
    lhs = list(type = "symbol", name = nm, nd = eq$lhs$nd,
               data_type = eq$lhs$data_type),
    rhs = list(type = "user", user = TRUE, user_dim = TRUE, default = FALSE),
    depends = NULL,
    expr = eq$expr,
    expr_str = eq$expr_str,
    line = eq$line,
    stage = eq$stage)
  dat$eqs[[nm]] <- eq

  nm_dim <- array_dim_name(nm)
  dat$eqs[[nm_dim]]$lhs$type <- "null"
  dat$eqs[[nm_dim]]$rhs$type <- "null"

  ## Then reorder things a little so that we are sure to process all
  ## dimensions before the compound ones.
  i <- which(names(dat$eqs) == nm)
  j <- which(names(dat$eqs) == nm_dim)
  stopifnot(i > j)
  k <- seq_along(dat$eqs)[-i]
  if (j == 1) {
    ## Avoids the -integer(0) indexing trap which deletes everything!
    k <- c(i, k)
  } else {
    k <- c(k[seq_len(j - 1)], i, k[-seq_len(j - 1)])
  }
  dat$eqs <- dat$eqs[k]

  dat
}


ir_prep_dim <- function(dat) {
  i <- dat$traits[, "is_dim"]
  if (any(i)) {
    dim_user <- unlist(lapply(dat$eqs[i], function(x)
      if (isTRUE(x$rhs$user)) x$lhs$name_target), FALSE, FALSE)
    ## Here we just need to expand out the equations and the
    ## associated other bits of the data (stage, deps_rec and traits).
    ## This is a fiddle but not that hard.  Once that's done, we might
    ## chase this through the generation then look at the user sized
    ## arrays which need the f1 part of this done differently (and
    ## that changes a few things but not a massive amount).
    tmp <- lapply(dat$eqs[i], ir_prep_dim1, dat)
    eqs <- unlist(tmp, FALSE, FALSE)
    names(eqs) <- vcapply(eqs, "[[", "name")
    eqs_len <- lengths(tmp, FALSE)

    ## So here we add everything - this is going to differ for the
    ## user dimension case again, but we can work around that as
    ## needed.
    traits <- dat$traits[rep(1, length(eqs)), , drop = FALSE]
    traits[] <- FALSE
    traits[, "is_symbol"] <- TRUE
    rownames(traits) <- names(eqs)
    stage <- viapply(eqs, "[[", "stage")

    deps <- function(depends) {
      sort(unique(c(depends, unlist(dat$deps_rec[depends], FALSE, FALSE))))
    }
    deps_rec <- lapply(eqs, function(x) deps(x$depends$variables))

    ## Now interleave:
    j <- ir_prep_interleave_index(i, eqs_len)
    dat$eqs <- c(dat$eqs, eqs)[j]
    dat$traits <- rbind(dat$traits, traits)[j, , drop = FALSE]

    dat$stage <- c(dat$stage[setdiff(names(dat$stage), names_if(i))], stage)
    dat$deps_rec <- c(dat$deps_rec[setdiff(names(dat$deps_rec), names_if(i))],
                      deps_rec)

    ## Here we need to move these to just before their respective
    ## assigments:
    for (nm in dim_user) {
      dat <- ir_prep_dim_user(nm, dat)
    }
  }
  dat
}


ir_prep_dim1 <- function(eq, dat) {
  rank <- eq$nd
  name <- eq$lhs$name_target
  user <- isTRUE(eq$rhs$user)
  dims <- lapply(seq_len(rank), function(i) as.name(array_dim_name(name, i)))

  f <- function(dim_name, value) {
    if (is.null(value)) {
      lhs_type <- "symbol"
      rhs_type <- "expression"
      depends <- find_symbols(as.name(name))
      args <- if (rank == 1) NULL else dims
      value <- as.call(c(list(quote(user), as.name(name)), args))
    } else if (identical(value, 0L)) {
      lhs_type <- rhs_type <- "null"
      depends <- NULL
    } else {
      lhs_type <- "symbol"
      rhs_type <- if (is.atomic(value)) "atomic" else "expression"
      depends <- find_symbols(value)
    }
    list(name = dim_name,
         lhs = list(type = lhs_type,
                    name = dim_name,
                    data_type = "int"),
         rhs = list(type = rhs_type,
                    value = value,
                    depends = depends),
         depends = depends,
         expr = eq$expr,
         expr_str = eq$expr_str,
         line = eq$line,
         stage = eq$stage)
  }

  ## Primary dimensions (will be done differently in the case of user
  ## dimensions)
  f1 <- function(i) {
    f(array_dim_name(name, i), eq$rhs$value[[i + 1]])
  }

  f2 <- function(i) {
    j <- seq_len(i - 1)
    name <- array_dim_name(name, paste(j, collapse = ""))
    value <- collapse_expr(dims[j], "*")
    f(name, value)
  }

  f3 <- function(i) {
    f(array_dim_name(name, i), 0L)
  }

  ## Looks like the rank 1 case needs special treatment here.
  if (rank == 1) {
    if (user) {
      ret <- list(f(array_dim_name(name), NULL))
    } else {
      ret <- list(f(array_dim_name(name), eq$rhs$value))
    }
  } else {
    if (user) {
      ret <- c(lapply(seq_len(rank), f3),
               list(f(array_dim_name(name), NULL)))
    } else {
      ret <- c(lapply(seq_len(rank), f1),
               list(f(array_dim_name(name), collapse_expr(dims, "*"))))
    }
  }

  if (rank >= 3) {
    ret <- c(ret, lapply(3:rank, f2))
  }

  is_internal <- !(name %in% c(dat$variable_info$order, dat$output_info$order))
  is_initial <- name %in% dat$variable_info$order
  if ((is_internal || is_initial) && !user) {
    name_target <- if (is_initial) initial_name(name) else name
    alloc <- call("alloc", as.name(array_dim_name(name)),
                  if (rank > 1) as.call(c(quote(c), dims)))
    depends <- find_symbols(alloc)
    depends$functions <- character(0)
    alloc_name <- sprintf("alloc_%s", name_target)
    eq_alloc <- list(
      name = alloc_name,
      lhs = list(type = "symbol",
                 name = name_target,
                 name_target = name_target,
                 data_type = eq$lhs$data_type),
      rhs = list(type = "alloc",
                 value = alloc,
                 depends = depends),
      depends = depends,
      expr = eq$expr,
      expr_str = eq$expr_str,
      line = eq$line,
      stage = eq$stage)
    ret <- c(ret, list(eq_alloc))
  }

  ret
}


ir_prep_interpolate <- function(x, dat) {
  stage_alloc <- max(dat$stage[x$depends$variables])
  stopifnot(stage_alloc <= STAGE_USER)

  x_alloc <- x
  x_alloc$name <- x$rhs$value$name
  x_alloc$lhs$name <- x$rhs$value$name
  x_alloc$lhs$name_target <- x$rhs$value$name
  x_alloc$stage <- stage_alloc
  x_alloc$lhs$data_type <- "interpolate_data" # (really void*)
  x_alloc$alloc_interpolate <- TRUE
  x_alloc$interpolate <- c(x$rhs$value[c("t", "y", "type")],
                           list(equation = x$name))
  x_alloc$depends$functions <- character(0)

  time <- if (dat$info$discrete) STEP else TIME
  x_use <- x
  x_use$lhs <- list(
    type = "symbol",
    name = x$lhs$name,
    data_type = x$lhs$data_type,
    nd = x$lhs$nd)
  x_use$rhs <- list(
    type = "expression",
    value = call("interpolate", as.name(x_alloc$lhs$name)))
  x_use$depends <- list(functions = character(),
                        variables = c(time, x$rhs$value$name))

  list(alloc = x_alloc, use = x_use)
}


ir_prep_delay <- function(dat) {
  if (dat$info$has_delay) {
    if (dat$info$discrete) {
      dat <- ir_prep_delay_discrete(dat)
    } else {
      dat <- ir_prep_delay_continuous(dat)
    }
  }
  dat
}


ir_prep_delay_continuous <- function(dat) {
  i <- !vlapply(dat$eqs, function(x) is.null(x$delay))
  tmp <- unname(lapply(dat$eqs[i], ir_prep_delay_continuous1, dat))

  eqs <- unlist(unname(tmp), FALSE, FALSE)
  names(eqs) <- vcapply(eqs, "[[", "name")

  j <- ir_prep_interleave_index(i, lengths(tmp, FALSE))

  ## This is mostly ignored for us now, so punt here:
  traits <- dat$traits[rep(which(i), each = 4), , drop = FALSE]
  rownames(traits) <- names(eqs)

  eqs_new <- eqs[!(names(eqs) %in% names(dat$eqs))]

  ## shared things:
  initial_time <- initial_name(TIME)

  eqs_common <- list(
    list(
      name = initial_time,
      lhs = list(type = "null", data_type = "int"),
      rhs = list(type = "null"),
      line = integer(0),
      stage = STAGE_USER,
      deps_rec = character(0)))
  names(eqs_common) <- vcapply(eqs_common, "[[", "name")

  traits_common <- traits[rep(1, length(eqs_common)), , drop = FALSE]
  traits_common[] <- FALSE
  traits_common[, "is_symbol"] <- TRUE
  rownames(traits_common) <- names(eqs_common)

  arr <- lapply(tmp, function(x) names_if(x$use$delay$expr$deps_is_array))
  arr <- unlist(arr, FALSE, FALSE)
  if (length(arr) > 0L) {
    f <- function(x) {
      nm <- sprintf("delay_arr_%s", x)
      list(
        name = nm,
        lhs = list(type = "delay_array", length = x, name = nm,
                   nd = dat$eqs[[x]]$lhs$nd,
                   data_type = dat$eqs[[x]]$lhs$data_type),
        depends = find_symbols(as.name(x)),
        deps_rec = c(x, dat$deps_rec[[x]]),
        line = dat$eqs[[x]]$line,
        stage = dat$eqs[[x]]$stage)
    }
    eqs_arr <- lapply(arr, f)
    names(eqs_arr) <- vcapply(eqs_arr, "[[", "name")

    traits_arr <- traits[rep(1, length(eqs_arr)), , drop = FALSE]
    traits_arr[] <- FALSE
    traits_arr[, "is_array"] <- TRUE
    rownames(traits_arr) <- names(eqs_arr)
  } else {
    eqs_arr <- NULL
    traits_arr <- NULL
  }

  dat$eqs <- c(c(dat$eqs, eqs)[j], eqs_common, eqs_arr)
  dat$traits <- rbind(rbind(dat$traits, traits)[j, , drop = FALSE],
                      traits_common, traits_arr)
  dat$stage <- c(dat$stage,
                 viapply(c(eqs_new, eqs_common, eqs_arr), "[[", "stage"))
  dat$deps_rec <- c(dat$deps_rec,
                    lapply(c(eqs_new, eqs_common, eqs_arr), "[[", "deps_rec"))
  dat
}


ir_prep_delay_discrete <- function(dat) {
  i <- !vlapply(dat$eqs, function(x) is.null(x$delay))
  tmp <- unname(lapply(dat$eqs[i], ir_prep_delay_discrete1, dat))

  eqs <- unlist(unname(tmp), FALSE, FALSE)
  names(eqs) <- vcapply(eqs, "[[", "name")
  j <- ir_prep_interleave_index(i, lengths(tmp, FALSE))

  ## This is mostly ignored for us now, so punt here:
  traits <- dat$traits[rep(which(i), each = 3), , drop = FALSE]
  rownames(traits) <- names(eqs)

  eqs_new <- eqs[!(names(eqs) %in% names(dat$eqs))]

  initial_time <- initial_name(STEP)

  eqs_common <- list(
    list(
      name = initial_time,
      lhs = list(type = "null", data_type = "int"),
      rhs = list(type = "null"),
      line = integer(0),
      stage = STAGE_USER,
      deps_rec = character(0)))
  names(eqs_common) <- vcapply(eqs_common, "[[", "name")

  traits_common <- traits[rep(1, length(eqs_common)), , drop = FALSE]
  traits_common[] <- FALSE
  traits_common[, "is_symbol"] <- TRUE
  rownames(traits_common) <- names(eqs_common)

  dat$eqs <- c(c(dat$eqs, eqs)[j], eqs_common)
  dat$traits <- rbind(rbind(dat$traits, traits)[j, , drop = FALSE],
                      traits_common)
  dat$stage <- c(dat$stage,
                 viapply(c(eqs_new, eqs_common), "[[", "stage"))
  dat$deps_rec <- c(dat$deps_rec,
                    lapply(c(eqs_new, eqs_common), "[[", "deps_rec"))
  dat
}


ir_prep_delay_continuous1 <- function(eq, dat) {
  nm <- eq$name
  nm_state <- sprintf("delay_%s_%s", STATE, nm)
  nm_idx <- sprintf("delay_idx_%s", nm)
  nm_dim <- array_dim_name(nm_state)

  stage <- eq$delay$expr$total_stage
  value_len <- eq$delay$expr$total
  depends_len <- find_symbols(value_len)
  deps_rec_len <- unique(unlist(dat$deps_rec[depends_len$variables]))
  deps_rec_other <- c(nm_dim, deps_rec_len)

  eq_len <- list(
    name = nm_dim,
    lhs = list(type = "symbol", name = nm_dim, data_type = "int"),
    rhs = list(type = if (is.atomic(value_len)) "atomic" else "expression",
               value = value_len),
    for_delay = TRUE, # this is me being lazy
    depends = find_symbols(value_len),
    deps_rec = deps_rec_len %||% character(0),
    line = eq$line,
    stage = stage)

  eq_idx <- list(
    name = nm_idx,
    lhs = list(type = "delay_index", name = nm_idx, data_type = "int",
               nd = 1L, length = nm_dim),
    for_delay = nm,
    depends = find_symbols(as.name(nm_dim)),
    deps_rec = deps_rec_other,
    line = eq$line,
    stage = stage)

  eq_state <- list(
    name = nm_state,
    lhs = list(type = "null", name = nm_state, data_type = "double",
               nd = 1L, length = nm_dim),
    rhs = list(type = "null"),
    for_delay = nm,
    depends = find_symbols(as.name(nm_dim)),
    deps_rec = deps_rec_other,
    line = eq$line,
    stage = stage)

  eq$delay$continuous <- TRUE
  eq$delay$expr$length <- nm_dim
  eq$delay$expr$state <- nm_state
  eq$delay$expr$index <- nm_idx

  eq$delay$expr$equations <-
    names_if(dat$stage[eq$delay$expr$deps] == STAGE_TIME)

  list(len = eq_len, idx = eq_idx, state = eq_state, use = eq)
}


ir_prep_delay_discrete1 <- function(eq, dat) {
  if (!is.null(eq$lhs$nd)) {
    browser()
  }

  nm <- eq$name
  nm_ring <- sprintf("delay_ring_%s", nm)
  nm_ring_alloc <- sprintf("delay_ring_%s_alloc", nm)

  stage <- eq$delay$expr$total_stage
  value_len <- eq$delay$expr$total
  deps_ring <- find_symbols(value_len)

  if (eq$lhs$data_type != "double") {
    stop("delayed non-doubles needs work")
  }

  ## if not a double, then throw here - we'll need to patch that up
  ## later, or just convert in and out.

  eq_alloc <- list(
    name = nm_ring_alloc,
    lhs = list(type = "alloc_ring", name = nm_ring,
               data_type = "ring_buffer", length = value_len,
               name_target = nm_ring),
    rhs = list(type = "alloc"),
    for_delay = nm,
    depends = find_symbols(value_len),
    line = eq$line,
    stage = stage)

  eq_push <- list(
    name = nm_ring,
    lhs = list(type = "delay_discrete_push", name = nm_ring,
               data_type = "ring_buffer"),
    rhs = list(value = eq$rhs$value_expr),
    depends = find_symbols(eq$rhs$value_expr),
    line = eq$line,
    stage = STAGE_TIME)

  eq_use <- list(
    name = nm,
    lhs = list(type = "delay_discrete_read", name = nm,
               data_type = eq$lhs$data_type,
               nd = eq$lhs$nd),
    delay = list(ring = nm_ring,
                 default = eq$delay$default,
                 time = eq$delay$time),
    depends = join_deps(list(find_symbols(as.name(nm_ring)),
                             find_symbols(eq$delay$default),
                             find_symbols(eq$delay$time))),
    line = eq$line,
    stage = STAGE_TIME)

  list(alloc = eq_alloc, push = eq_push, use = eq_use)
}


ir_config <- function(dat) {
  ## We should probably add here:
  ##
  ## - odin version
  ## - file
  ## - type
  ## - path
  ## - include?
  list(base = jsonlite::unbox(dat$config$base))
}


ir_meta <- function(dat) {
  discrete <- dat$info$discrete
  time <- if (discrete) STEP else TIME
  result <- if (discrete) STATE_NEXT else DSTATEDT
  meta <- c(internal = INTERNAL,
            user = USER,
            state = STATE,
            result = result,
            output = OUTPUT,
            time = time,
            initial_time = initial_name(time))
  lapply(meta, jsonlite::unbox)
}


ir_features <- function(dat) {
  v <- c("discrete", "has_array", "has_output", "has_user", "has_delay",
         "has_interpolate", "has_stochastic")
  ret <- lapply(dat$info[v], jsonlite::unbox)
  ret$initial_time_dependent <-
    jsonlite::unbox(dat$info$initial_stage == STAGE_TIME)
  ret
}


ir_equations <- function(dat) {
  ## TODO: there are not actually any equations marked 'exclude'
  ## anymore, but we are using 'null' equations here to try and filter
  ## equations in the case where they're set by a side-effect of a
  ## library function.
  exclude <- vlapply(dat$eqs, function(x) isTRUE(x$exclude)) |
    vcapply(dat$eqs, function(x) x$lhs$type) == "null"
  unname(lapply(dat$eqs[!exclude], ir_equation))
}


ir_components <- function(dat) {
  v <- c("create", "user", "initial", "rhs", "output")
  f <- function(i) {
    list(variables = dat$variable_info$used[[i]] %||% character(0),
         equations = dat$evaluated[[i]] %||% character(0))
  }
  set_names(lapply(v, f), v)
}


ir_user <- function(dat) {
  ## Ensure that these are always ordered by has default so that
  ## required parameters are listed first.
  user <- dat$info$user
  user <- user[order(user$has_default), ]
  stopifnot(identical(rownames(user), user$name))

  ## TODO: holding off on using min, max and integer because these
  ## belong more in the data declaration I think; we can use similar
  ## approaches to declare other constraints perhaps.
  f <- function(i) {
    list(name = jsonlite::unbox(user$name[[i]]),
         has_default = jsonlite::unbox(user$has_default[[i]]))
  }

  lapply(seq_len(nrow(user)), f)
}


ir_interpolate <- function(dat) {
  eqs <- dat$eqs[vlapply(dat$eqs, function(x) isTRUE(x$rhs$interpolate))]
  time <- vcapply(eqs, function(x) x$interpolate$t, USE.NAMES = FALSE)
  type <- vcapply(eqs, function(x) x$interpolate$type, USE.NAMES = FALSE)
  list(min = time,
       max = time[type != "constant"],
       critical = time[type == "constant"])
}


ir_equation <- function(eq) {
  if (isTRUE(eq$rhs$user)) {
    return(ir_equation_user(eq))
  } else if (identical(eq$rhs$type, "alloc")) {
    return(ir_equation_alloc(eq))
  } else if (isTRUE(eq$alloc_interpolate)) {
    return(ir_equation_alloc_interpolate(eq))
  } else if (isTRUE(eq$rhs$output_self)) {
    return(ir_equation_copy(eq))
  } else if (identical(eq$lhs$type, "delay_index")) {
    return(ir_equation_delay_index(eq))
  } else if (identical(eq$lhs$type, "delay_array")) {
    return(ir_equation_delay_array(eq))
  } else if (isTRUE(eq$rhs$delay) && eq$delay$continuous) {
    return(ir_equation_delay_continuous(eq))
  } else if (identical(eq$lhs$type, "symbol")) {
    return(ir_equation_expression_scalar(eq))
  } else if (identical(eq$lhs$type, "array")) {
    return(ir_equation_expression_array(eq))
  } else if (identical(eq$lhs$type, "alloc_ring")) {
    return(ir_equation_alloc_ring(eq))
  } else if (identical(eq$lhs$type, "delay_discrete_push")) {
    return(ir_equation_delay_discrete_push(eq))
  } else if (identical(eq$lhs$type, "delay_discrete_read")) {
    return(ir_equation_delay_discrete_read(eq))
  } else {
    stop("Unclassified type")
  }
}


ir_equation_lhs <- function(eq) {
  if ((is.null(eq$lhs$special) || eq$lhs$special == "initial") &&
      !identical(eq$rhs$type, "alloc") &&
      !isTRUE(eq$alloc_interpolate)) {
    target <- eq$name
  } else {
    target <- eq$lhs$name_target
  }
  jsonlite::unbox(target)
}


ir_equation_base <- function(type, eq, ...) {
  list(name = jsonlite::unbox(eq$name),
       type = jsonlite::unbox(type),
       source = unname(eq$line),
       depends = ir_depends(eq$depends),
       lhs = ir_equation_lhs(eq),
       ...)
}


ir_equation_expression_scalar <- function(eq) {
  rhs <- list(
    value = ir_expression(eq$rhs$value))
  ir_equation_base("expression_scalar", eq, rhs = rhs)
}


ir_equation_expression_array <- function(eq) {
  if (any(eq$rhs$inplace)) {
    stop("rhs$inplace")
  }
  rhs <- lapply(seq_along(eq$lhs$index), ir_equation_expression_array_rhs, eq)
  ir_equation_base("expression_array", eq, rhs = rhs)
}


ir_equation_expression_array_rhs <- function(i, eq) {
  index <- eq$lhs$index[[i]]
  list(
    index = lapply(seq_along(index$value), function(j)
      list(value = ir_expression(index$value[[j]]),
           is_range = jsonlite::unbox(index$is_range[[j]]),
           index = jsonlite::unbox(INDEX[[j]]))),
    value = ir_expression(eq$rhs$value[[i]]))
}


ir_equation_delay_index <- function(eq) {
  ir_equation_base("delay_index", eq,
                   delay = jsonlite::unbox(eq$for_delay))
}


ir_equation_delay_array <- function(eq) {
  ir_equation_base("alloc", eq)
}


ir_equation_delay_continuous <- function(eq) {
  ## TODO: for now assuming continuous; this totally changes for
  ## discrete system.
  rhs <- list(value = ir_expression(eq$rhs$value_expr))
  if (!is.null(eq$lhs$nd) && eq$lhs$nd > 0) {
    index1 <- function(i) {
      if (eq$lhs$nd == 1L) {
        len <- list(jsonlite::unbox("length"), jsonlite::unbox(eq$name))
      } else {
        len <- list(jsonlite::unbox("dim"), jsonlite::unbox(eq$name),
                    jsonlite::unbox(i))
      }
      list(value = list(jsonlite::unbox(":"), jsonlite::unbox(1), len),
           is_range = jsonlite::unbox(TRUE),
           index = jsonlite::unbox(INDEX[[i]]))
    }
    rhs$index <- lapply(seq_len(eq$lhs$nd), index1)
  }

  info <- eq$delay$expr
  if (any(info$deps_is_array)) {
    arr <- names_if(info$deps_is_array)
    ## TODO: this should be *time sensitive* arrays only.
    if (length(setdiff(names(arr), info$equations)) > 0L) {
      stop("FIXME")
    }
    substitutions <- lapply(arr, function(i)
      list(from = jsonlite::unbox(i),
           to = jsonlite::unbox(sprintf("delay_arr_%s", arr))))
  } else {
    substitutions <- list()
  }

  ## For now at least, we need to substitute out the offsets here,
  ## because we're not creating appropriate variables for them.  This
  ## will likely change later on, but for now can't be helped without
  ## complicating things even more.
  info$offset <- variable_offsets2(info$order, info$is_array, info$len)

  contents <- lapply(seq_along(info$order), function(i)
    list(name = jsonlite::unbox(info$order[[i]]),
         offset = ir_expression(info$offset[[i]])))
  delay <- list(state = jsonlite::unbox(info$state),
                index = jsonlite::unbox(info$index),
                substitutions = substitutions,
                variables = list(
                  length = jsonlite::unbox(info$length),
                  contents = contents),
                equations = info$equations,
                default = ir_expression(eq$delay$default$value),
                time = ir_expression(eq$delay$time))


  ir_equation_base("delay_continuous", eq, rhs = rhs, delay = delay)
}


ir_equation_alloc <- function(eq) {
  ir_equation_base("alloc", eq)
}


ir_equation_copy <- function(eq) {
  ir_equation_base("copy", eq)
}


ir_equation_alloc_interpolate <- function(eq) {
  interpolate <- lapply(eq$interpolate, jsonlite::unbox)
  ir_equation_base("alloc_interpolate", eq, interpolate = interpolate)
}


ir_equation_alloc_ring <- function(eq) {
  ir_equation_base("alloc_ring", eq, delay = jsonlite::unbox(eq$for_delay))
}


ir_equation_delay_discrete_push <- function(eq) {
  rhs <- list(value = ir_expression(eq$rhs$value))
  ir_equation_base("delay_discrete_push", eq, rhs = rhs)
}


ir_equation_delay_discrete_read <- function(eq) {
  delay <- list(ring = jsonlite::unbox(eq$delay$ring),
                time = ir_expression(eq$delay$time),
                default = ir_expression(eq$delay$default))
  ir_equation_base("delay_discrete_read", eq, delay = delay)
}


ir_equation_user <- function(eq) {
  if (!is.null(eq$rhs$integer) || !is.null(eq$rhs$min) ||
      !is.null(eq$rhs$max)) {
    stop("User details need supporting")
  }
  user <- list(
    default = if (eq$rhs$default) ir_expression(eq$rhs$value) else NULL,
    dim = jsonlite::unbox(isTRUE(eq$rhs$user_dim)))
  ir_equation_base("user", eq, user = user)
}


ir_depends <- function(x) {
  if (length(x$functions) == 0L && length(x$variables) == 0L) {
    NULL
  } else {
    x
  }
}


ir_expression <- function(expr) {
  if (is.symbol(expr)) {
    jsonlite::unbox(as.character(expr))
  } else if (is.atomic(expr)) {
    jsonlite::unbox(expr)
  } else if (is.call(expr)) {
    c(list(jsonlite::unbox(as.character(expr[[1L]]))),
      lapply(expr[-1L], ir_expression))
  } else {
    stop("implement me")
  }
}


ir_data <- function(dat) {
  ## quickly patch up the data because otherwise this is a pain to read:
  for (i in seq_along(dat$eqs)) {
    eq <- dat$eqs[[i]]
    if (identical(eq$lhs$special, "deriv") ||
        identical(eq$lhs$special, "update") ||
        identical(eq$lhs$special, "output")) {
      dat$eqs[[i]]$name <- eq$lhs$name_target
      dat$eqs[[i]]$lhs$name <- eq$lhs$name_target
    }
    is_transient <- eq$lhs$location == "internal" &&
      !identical(eq$rhs$type, "alloc") &&
      !identical(eq$lhs$type, "delay_array") &&
      !identical(eq$lhs$type, "delay_discrete_push") &&
      (eq$stage == STAGE_TIME && is.null(eq$lhs$nd) &&
       !identical(eq$lhs$special, "initial"))
    if (is_transient) {
      dat$eqs[[i]]$lhs$location <- "transient"
    }
  }
  i <- !vlapply(dat$eqs, function(x) identical(x$rhs$type, "alloc"))
  data <- lapply(unname(dat$eqs[i]), function(eq)
    list(name = jsonlite::unbox(eq$name),
         location = jsonlite::unbox(eq$lhs$location),
         storage_type = jsonlite::unbox(eq$lhs$data_type),
         rank = jsonlite::unbox(eq$lhs$nd %||% 0L),
         dimnames = ir_dimnames(eq$lhs$length %||% eq$lhs$name, eq$lhs$nd)))
  list(data = data,
       variable = ir_data_variable(dat, FALSE),
       output = ir_data_variable(dat, TRUE))
}


ir_data_variable <- function(dat, output) {
  info <- if (output) dat$output_info else dat$variable_info
  if (output && !dat$info$has_output) {
    info <- list(total = 0)
  }
  offset <- set_names(info$offset, info$order)
  contents <- lapply(seq_along(info$order), function(i)
    list(name = jsonlite::unbox(info$order[[i]]),
         offset = ir_expression(info$offset[[i]])))
  ## Only for output:
  if (!output) {
    for (i in seq_along(contents)) {
      contents[[i]]$initial <- jsonlite::unbox(initial_name(info$order[[i]]))
    }
  }
  list(length = ir_expression(info$total),
       contents = contents)
}


ir_dimnames <- function(name, rank) {
  if (is.null(rank) || rank == 0L) {
    return(NULL)
  }
  ## This is a hack for the delay hack
  if (grepl("^dim_", name)) {
    length <- jsonlite::unbox(name)
    stopifnot(rank == 1L)
  } else {
    length <- jsonlite::unbox(array_dim_name(name))
  }

  if (rank == 1L) {
    dim <- NULL
  } else {
    dim <- vcapply(seq_len(rank), array_dim_name, name = name)
  }
  if (rank == 1) {
    mult <- NULL
  } else {
    mult <- c("", vcapply(2:rank, function(i)
      array_dim_name(name, paste(seq_len(i - 1), collapse = ""))))
  }
  list(length = length, dim = dim, mult = mult)
}


## For delays, based on code in parse.R
variable_offsets2 <- function(names, is_array, len) {
  n <- length(names)
  offset <- as.list(seq_len(n) - 1L)
  accumulate_offset <- function(i) {
    if (i == 1L) {
      0L
    } else if (!is_array[[i - 1L]]) {
      offset[[i - 1L]] + 1L
    } else if (identical(offset[[i - 1L]], 0L)) {
      as.name(array_dim_name(names[[i - 1L]]))
    } else {
      prev <- offset[[i - 1L]]
      call("+", prev, as.name(len[[i - 1L]]))
    }
  }
  for (i in which(is_array)) {
    offset[[i]] <- accumulate_offset(i)
  }

  offset
}
