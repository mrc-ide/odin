## TODO: Disallow '<base>_' as a name; otherwise potential for
## collision, so probably set that in the DSL rather than here.
odin_generate1 <- function(dat, safe) {
  obj <- odin_generate1_object(dat, safe)

  ## (1): Before really starting, add a few elements to the object.
  ##
  ## Flag indicating if we're using dde, and for saving initial time.
  ##
  ## These must both exist before we hit any delay equation, and will
  ## be used in the second pass unconditionally.  The internal
  ## interface here may change in future.
  ##
  ## The dde flag is set at object creation and a default cannot (yet)
  ## be set from within the odin code [TODO]
  if (!dat$info$discrete) {
    obj$add_element("odin_use_dde", "int")
  }

  if (obj$info$has_delay) {
    initial_time <- initial_name(if (obj$info$discrete) STEP else TIME)
    obj$add_element(initial_time, "double")
    if (obj$info$discrete) {
      obj$add_element(RING, "ring_buffer")
    }
  }

  odin_generate1_library(obj, dat$eqs)

  ## The main loop over all equations:
  odin_generate1_loop(obj, dat$eqs)

  ## This might need pulling out into something else as it's a bit
  ## unweildly in here.
  if (obj$info$discrete && obj$info$has_delay) {
    info <- obj$discrete_delay_info
    len <- if (info$total_is_var) call("(", info$total) else info$total
    st <- STAGES[[info$total_stage]]
    mxhist <- 10000 # TODO: make configurable, perhaps same way as use_dde?
    obj[[st]]$add("%s = ring_buffer_create(%s, %s * sizeof(double), %s);",
                  obj$rewrite(RING),
                  obj$rewrite(mxhist),
                  obj$rewrite(len),
                  "OVERFLOW_OVERWRITE")
    obj$free$add("ring_buffer_destroy(%s);", obj$rewrite(RING))
    if (st != "constant") {
      ## This is not hard; just need to reallocate the memory
      ## however I usually do it (can be Free/Calloc)
      stop("FIXME (dynamic ring allocation)")
    }
  }

  odin_generate1_total(obj$variable_info, obj)
  if (obj$info$has_output) {
    odin_generate1_total(obj$output_info, obj)
  }

  ## By this point, all variables have been added.
  obj$add_element <- function(...) stop("odin bug")

  ## We're going to use this in a couple of places and it's kind of
  ## awkward.  In contrast with vars, which is known on entry to this
  ## function, types is collected by this function so needs to be
  ## written out at the end.
  obj$types <- rbind_as_df(obj$types$get())

  obj
}

## What we are going to write here is a little bit of (fairly nasty)
## reference-style code that will help generate the interface.  The
## simplest way of doing this might be to generate a small list of
## types that we can reference everywhere.  But that might just be
## more complicated than it needs to be?
odin_generate1_object <- function(dat, safe) {
  base <- dat$info$base
  self <- list(base = base, safe = safe)

  self$info <- dat$info

  ## TODO: This might change once I get the proof of concept working.
  ## This could also be the core of customising output things a bit.
  ##
  ## TODO: I really don't think that target_name is good here
  self$core <- dat$info["target_name"]
  if (self$info$discrete) {
    self$core$state2 <- STATE_NEXT
  } else {
    self$core$state2 <- DSTATEDT
  }

  self$variable_info <- dat$variable_info
  self$output_info <- dat$output_info
  self$discrete_delay_info <- dat$discrete_delay_info

  self$name_pars <- sprintf("%s_p", base)
  self$type_pars <- sprintf("%s_pars", base)

  ## The major stages; we'll collect things into these as the main
  ## loop runs.
  self$constant <- collector_named()
  self$user <- collector_named()
  self$time <- collector_named(TRUE)

  ## Used on initialisation/freeing
  self$initial <- collector()
  self$free <- collector()

  ## Keep track of which library functions we need.  I'll keep those
  ## elsewhere and select them based on name.
  self$library_fns <- collector()

  ## Collects only interpolation type information
  self$interpolate <- collector_list()

  ## Below here, things are related to each other; lookup, and types
  ## are both called by add_element, and rewrite looks up variables in
  ## lookup.  So as elements are added to the object (which happens in
  ## topological order) we can rewrite the C to pull them from the
  ## struct.

  ## This is the set of variables we know to be *ours*.
  self$lookup <- collector()

  ## Type information will generate a bunch of extra things, so
  ## process that later for simplicity:
  self$types <- collector_list()

  self$add_element <- function(name, type, array = 0, discrete_delay = FALSE) {
    if (array > 0L) {
      name_dim <- array_dim_name(name, use = FALSE)
      if (nzchar(name_dim)) {
        Recall(name_dim, "int", FALSE)
        if (array > 1L) {
          for (i in seq_len(array)) {
            Recall(array_dim_name(name, i, use = FALSE), "int")
          }
          if (array >= 3) {
            for (i in 3:array) {
              tmp <- paste(seq_len(i - 1), collapse = "")
              Recall(array_dim_name(name, tmp, use = FALSE), "int")
            }
          }
        }
      }
    }
    ## discrete delay (arrays) are treated differently; we don't
    ## actually add them into the structure, though we do add all the
    ## machinery for computing array sizes.  The approach here might
    ## change though.
    if (!discrete_delay) {
      self$types$add(list(name = name, type = type, array = array))
      self$lookup$add(name)
    }
  }

  ## Custom functions, defined in .c files:
  self$custom <- dat$config$include

  ## Rewrite based on all the above; only lookup is modified as we go.
  self$rewrite <- function(x) {
    rewrite_c(x, self$name_pars, self$lookup$get(), self$safe)
  }

  self
}

odin_generate1_loop <- function(obj, eqs) {
  for (x in eqs) {
    if (identical(x$lhs$special, "dim")) {
      odin_generate1_dim(x, obj)
    } else if (isTRUE(x$rhs$interpolate)) {
      odin_generate1_interpolate(x, obj)
    } else if (isTRUE(x$rhs$delay)) {
      odin_generate1_delay(x, obj, eqs)
    } else if (x$lhs$type == "symbol") {
      odin_generate1_symbol(x, obj)
    } else if (x$lhs$type == "array") {
      odin_generate1_array(x, obj, eqs)
    } else {
      stop("Unhandled type [odin bug]") # nocov
    }
  }
}

odin_generate1_total <- function(x, obj) {
  if (x$total_is_var) {
    obj$add_element(x$total_use, "int")
    st <- STAGES[[x$total_stage]]
    obj[[st]]$add("%s = %s;",
                  obj$rewrite(x$total_use),
                  obj$rewrite(x$total))
  }
}

## Support for any library functions we detect
odin_generate1_library <- function(obj, eqs) {
  ## Support for interacting with deSolve parameters
  obj$library_fns$add("get_ds_pars")

  ## Support for sum() of varying orders
  fn_deps <- function(x) {
    c(x$depends$functions, x$rhs$depends_delay$functions)
  }
  used_functions <- unique(unlist(lapply(eqs, fn_deps), use.names = FALSE))

  obj$library_fns$add(intersect(FUNCTIONS_SUM, used_functions))
  if ("sum" %in% used_functions) {
    obj$library_fns$add("odin_sum1")
  }
  if ("%%" %in% used_functions) {
    obj$library_fns$add("fmodr")
  }
  if ("%/%" %in% used_functions) {
    obj$library_fns$add("fintdiv")
  }

  ## Support for delay differential and difference equations:
  if (obj$info$has_delay) {
    if (obj$info$discrete) {
      obj$library_fns$add("lagvalue_discrete")
    } else {
      obj$library_fns$add("lagvalue_dde")
      obj$library_fns$add("lagvalue_ds")
    }
  }

  if (obj$safe) {
    obj$library_fns$add("odin_array_at1")
    obj$library_fns$add("odin_array_at_set1")
 }
}

odin_generate1_dim <- function(x, obj) {
  nm <- x$name
  nm_target <- x$lhs$name_target
  is_var <- nm_target %in% obj$variable_info$order
  is_output <- nm_target %in% obj$output_info$order
  is_discrete_delay <-
    obj$info$discrete && nm_target %in% obj$discrete_delay_info$order
  nm_s <- if (is_var) initial_name(nm_target) else nm_target
  st <- STAGES[[x$stage]]
  data_type <- x$lhs$data_type_target

  ## this sets up the storage and declares the dimensions for our array:
  obj$add_element(nm_s, data_type, x$nd, is_discrete_delay)

  if (x$stage == STAGE_USER) {
    obj$constant$add("%s = NULL;", obj$rewrite(nm_s))
    obj$user$add("Free(%s);", obj$rewrite(nm_s))
  } else if (x$stage > STAGE_USER) {
    stop("This should never happen! [odin bug]") # nocov
  }

  if (x$nd > 1L) {
    obj$library_fns$add("odin_set_dim")
  }

  if (isTRUE(x$rhs$user)) {
    ## Here, we'll need to a little extra work; get the value, check
    ## length, copy out integers.
    obj$library_fns$add("get_user_array_dim")
    obj$library_fns$add("get_user_array_check_rank")
    obj$library_fns$add("get_user_array_copy")

    ## We really need to do this in a scoped block I think as we need
    ## to set a few things all at once.
    obj[[st]]$add("{")

    is_real_str <- if (data_type == "double") "true" else "false"

    if (x$nd == 1L) {
      ## For the 1d/vector case we can just pass in the address of the single
      ## dimension.
      nm_i <- obj$rewrite(nm)
      obj[[st]]$add(
                 '  %s *tmp = (%s*)get_user_array_dim(%s, "%s", %s, %d, &%s);',
                 data_type, data_type, USER, nm_s, is_real_str, x$nd, nm_i)
    } else {
      ## TODO: Here and elsewhere, should all 'tmp' variables be
      ## odin_tmp?
      ##
      ## But for the matrix/array case we need to pass in space for
      ## all the dimensions.
      obj[[st]]$add("  int tmp_dim[%d];", x$nd)
      obj[[st]]$add(
                 '  %s *tmp = get_user_array_dim(%s, "%s", %s, %d, tmp_dim);',
                 data_type, USER, nm_s, is_real_str, x$nd)

      nm_i <- vcapply(seq_len(x$nd),
                      function(i) obj$rewrite(array_dim_name(nm_target, i)))
      obj[[st]]$add('  %s = tmp_dim[%d];', nm_i, seq_len(x$nd) - 1L)
      obj[[st]]$add(indent(
                 odin_generate1_dim_array_dimensions(x, obj$rewrite), 2L))
    }

    obj[[st]]$add("  %s = (%s*) Calloc(%s, %s);",
                  obj$rewrite(nm_s), data_type, obj$rewrite(nm), data_type)
    obj[[st]]$add("  memcpy(%s, tmp, %s * sizeof(%s));",
                  obj$rewrite(nm_s), obj$rewrite(nm), data_type)
    obj[[st]]$add("  Free(tmp);")
    obj[[st]]$add("}")
  } else {
    if (x$nd == 1) {
      obj[[st]]$add("%s = %s;", obj$rewrite(nm), obj$rewrite(x$rhs$value))
    } else {
      size <- as.list(x$rhs$value[-1L])
      for (i in seq_len(x$nd)) {
        obj[[st]]$add("%s = %s;",
                      obj$rewrite(array_dim_name(nm_target, i)),
                      obj$rewrite(size[[i]]))
      }
      obj[[st]]$add(odin_generate1_dim_array_dimensions(x, obj$rewrite))
    }
    if (!is_discrete_delay) {
      obj[[st]]$add("%s = (%s*) Calloc(%s, %s);",
                    obj$rewrite(nm_s), data_type, obj$rewrite(nm), data_type)
    }
  }
  if (!is_discrete_delay) {
    obj$free$add("Free(%s);", obj$rewrite(nm_s))
  }

  if (isTRUE(x$used_in_delay)) {
    ## Storage for array components that are used within delay
    ## expressions.  These will need to be extracted twice (once for
    ## "now", once in a delay expression) so we need to create a bit
    ## of extra space for them.
    nm_delay <- delay_name(nm_target)
    ## TODO: I'm not sure about the 1 on the end here; why is this not
    ## the correct nd here?  Just to avoid writing out all the
    ## subindexing?
    ##
    if (x$lhs$data_type == "int") {
      stop("odin bug - this should never happen") # nocov
    }
    obj$add_element(nm_delay, "double", 1L)
    if (x$stage == STAGE_USER) {
      obj$constant$add("%s = NULL;", obj$rewrite(nm_delay))
      obj$user$add("Free(%s);", obj$rewrite(nm_delay))
    }
    obj[[st]]$add("%s = (double*) Calloc(%s, double);",
                  obj$rewrite(nm_delay), obj$rewrite(nm))
    obj$free$add("Free(%s);", obj$rewrite(nm_delay))
  }

  if (is_var) {
    ## TODO: Does this not create some unnecessary offsets?  (e.g., in
    ## the mixed example in test-odin).  I would have thought that the
    ## *first* array would not need an offset; we'd only be interested
    ## in this if is.language(obj$variable_info$offset_use[[i]]) is
    ## TRUE?
    nm_offset <- offset_name(nm_target)
    obj$add_element(nm_offset, "int")
    i <- match(nm_target, obj$variable_info$order)
    offset <- obj$variable_info$offset[[i]]
    obj[[st]]$add("%s = %s;", obj$rewrite(nm_offset), obj$rewrite(offset))
  }

  if (is_output) {
    nm_offset <- offset_name(nm_target, TRUE)
    obj$add_element(nm_offset, "int")
    i <- match(nm_target, obj$output_info$order)
    offset <- obj$output_info$offset[[i]]
    obj[[st]]$add("%s = %s;", obj$rewrite(nm_offset), obj$rewrite(offset))
  }
}

odin_generate1_symbol <- function(x, obj) {
  st <- STAGES[[x$stage]]
  nm <- x$name
  data_type <- x$lhs$data_type

  is_initial <- identical(x$lhs$special, "initial")
  if (x$stage < STAGE_TIME || is_initial) {
    obj$add_element(nm, data_type)
  }

  if (isTRUE(x$rhs$user)) {
    if (isTRUE(x$rhs$default)) {
      default <- obj$rewrite(x$rhs$value)
    } else {
      default <- if (data_type == "int") "NA_INTEGER" else "NA_REAL"
    }
    obj$constant$add("%s = %s;", obj$rewrite(nm), default)
  }

  obj[[st]]$add(odin_generate1_symbol_expr(x, obj), name=nm)
}

odin_generate1_symbol_expr <- function(x, obj) {
  nm <- x$name
  data_type <- x$lhs$data_type

  if (isTRUE(x$rhs$user)) {
    get_user <- sprintf("get_user_%s", data_type)
    obj$library_fns$add(get_user)
    value <- sprintf("%s(%s, \"%s\", %s)", get_user, USER, nm, obj$rewrite(nm))
  } else if (isTRUE(x$rhs$output_self)) {
    value <- obj$rewrite(x$lhs$name_target)
  } else {
    value <- obj$rewrite(x$rhs$value)
  }

  is_initial <- identical(x$lhs$special, "initial")
  if (x$stage < STAGE_TIME || is_initial) {
    ret <- sprintf("%s = %s;", obj$rewrite(nm), value)
  } else if (identical(x$lhs$special, "deriv") ||
             identical(x$lhs$special, "update")) {
    ## NOTE: offset guaranteed to be OK, but should probably rewrite?
    i <- match(x$lhs$name_target, obj$variable_info$order)
    offset <- obj$rewrite(obj$variable_info$offset_use[[i]])
    ret <- sprintf("%s[%s] = %s;", obj$core$state2, offset, value)
  } else if (identical(x$lhs$special, "output")) {
    i <- match(x$lhs$name_target, obj$output_info$order)
    offset <- obj$rewrite(obj$output_info$offset_use[[i]])
    ret <- sprintf("%s[%s] = %s;", OUTPUT, offset, value)
  } else {
    ret <- sprintf("%s %s = %s;", data_type, nm, value)
  }
  ret
}

odin_generate1_array <- function(x, obj, eqs) {
  st <- STAGES[[x$stage]]
  nm <- x$name
  data_type <- x$lhs$data_type

  if (isTRUE(x$rhs$user)) {
    dim <- eqs[[array_dim_name(x$name)]]
    if (isTRUE(dim$rhs$user)) {
      ## All done already while establishing dim
      return()
    }
    nd <- dim$nd
    obj$library_fns$add("get_user_array")
    obj$library_fns$add("get_user_array_check_rank")
    obj$library_fns$add("get_user_array_copy")

    if (nd == 1) {
      dn <- obj$rewrite(dim$name)
    } else {
      dn <- paste(vcapply(seq_len(nd), function(i)
        obj$rewrite(array_dim_name(x$name, i)), USE.NAMES=FALSE), collapse=", ")
    }
    is_real_str <- if (data_type == "double") "true" else "false"
    obj[[st]]$add('get_user_array(%s, "%s", %s, %s, %d, %s);',
                  USER, nm, is_real_str, obj$rewrite(x$name), nd, dn, name=nm)
  } else if (isTRUE(x$rhs$output_self)) {
    obj[[st]]$add("memcpy(%s, %s, %s * sizeof(%s));",
                  obj$rewrite(x$name), obj$rewrite(x$lhs$name_target),
                  obj$rewrite(array_dim_name(x$lhs$name_target)),
                  data_type,
                  name=nm)
  } else {
    obj[[st]]$add(odin_generate1_array_expr(x, obj), name=nm)
  }
}

odin_generate1_array_expr <- function(x, obj) {
  ret <- collector()
  indent <- ""
  ## TODO: For >= 2 dimensions, consider running the indices
  ## backwards here to be more cache friendly.  So this means
  ## running k, then j, then i.  To do this well, store the offsets
  ## at each loop level.  See odin_sum2 and odin_sum3 for how this
  ## works.  I'm not really sure if this will actually make a
  ## performance gain, but it should be simple enough to implement.
  ## It's worth checking the code that compiler generates though
  ## actually differs (especially with -O2 or higher).
  ##
  ## TODO: For some models, we might need to allow control over how
  ## the sum indies are computed.
  for (j in seq_along(x$lhs$index)) {
    xj <- x$lhs$index[[j]]
    is_range <- xj$is_range
    target <- xj$extent_max
    ## TODO: The index variables need sanitising so that no more
    ## than one of i,j,k is allowed; things like x[i,j] = z[i + j]
    ## are not allowed! (this would be in odin_parse_arrays_check_rhs)
    for (k in seq_along(is_range)) {
      if (is_range[k]) {
        ret$add("%sfor (int %s = %s; %s < %s; ++%s) {",
                indent,
                INDEX[[k]], minus1(xj$extent_min[[k]], obj$rewrite),
                INDEX[[k]], obj$rewrite(xj$extent_max[[k]]),
                INDEX[[k]])
        indent <- paste0("  ", indent)
        target[[k]] <- as.symbol(INDEX[[k]])
      } else if (INDEX[[k]] %in% x$rhs$depends$variables) {
        if (!nzchar(indent)) {
          ret$add("{")
          indent <- "  "
        }
        ret$add("%sint %s = %s;", indent, INDEX[[k]],
                minus1(xj$extent_max[[k]], obj$rewrite))
        target[[k]] <- as.symbol(INDEX[[k]])
      } else {
        target[[k]] <- xj$extent_max[[k]]
      }
    }

    c_target <- obj$rewrite(as.call(c(quote(`[<-`), as.symbol(x$name), target)))
    c_value <- obj$rewrite(x$rhs$value[[j]])
    if (obj$safe) {
      ## Quite a different path here to avoid assigning to a function
      ## call
      ret$add("%s%s;", indent, sprintf(c_target, c_value))
    } else {
      ret$add("%s%s = %s;", indent, c_target, c_value)
    }

    while (nzchar(indent)) {
      indent <- substr(indent, 3L, nchar(indent))
      ret$add("%s}", indent)
    }
  }
  ret$get()
}

odin_generate1_delay <- function(x, obj, eqs) {
  if (obj$info$discrete) {
    odin_generate1_delay_discrete(x, obj, eqs)
  } else {
    odin_generate1_delay_ode(x, obj, eqs)
  }
}

## TODO: I think I have some duplication here in both the variable
## ordering and the output ordering (computing offsets and the like).
## Here I'm hoping to make the simplifying assumption that the number
## of array variables stored is smallish so we can afford to manually
## do calculations on them.
##
## TODO: This is, by far, the ugliest function.  It would probably pay
## to split it up a bit and leverage any of the other generate bits I
## can find where duplicated code is involved.  But this is *hard* in
## general.
odin_generate1_delay_ode <- function(x, obj, eqs) {
  nm <- x$name
  time_name <- TIME
  time_type <- "double"

  delay_len_tot <- obj$rewrite(x$delay$expr$total)

  ## NOTE: There's some duplication here in terms of the size of
  ## various arrays.  That helps with some assumptions about how
  ## the array allocations and copying works.  It's possible that
  ## could be worked around with a #define but I don't think it's
  ## worth it at this point.  For now just accept the redundancy.
  ##
  ## NOTE: it would be *heaps* simpler to extract the entire
  ## structure here (create one vector of length `p->dim` and set
  ## it as `0..(p->dim-1)` but potentially slower as it will look
  ## up all varaibles (and most of the time we won't be interested
  ## in all).
  ##
  ## TODO: The Calloc/Free calls here could move into contents if
  ## it took a stage argument.
  ##
  ## TODO: The Calloc/Free calls here are in danger of being incorrect
  ## here with user-sized array input.  I think that this just
  ## requires nailing the stage correctly, but there are some unusual
  ## dependencies for delay things (and we need to delay on the size
  ## for all given variables which is not currently supported
  ## properly)

  ## The options for naming are to use a series of indices (e.g., 1,
  ## 2, 3) or name things after the variables that are being delayed
  ## (delay_1_idx vs delay_lag_inf_idx, delay_1_state vs
  ## delay_lag_inf_state).  I think that the latter is probably nicer.
  ##
  ## TODO: What on earth is going on with delay_idx here?
  delay_idx <- delay_name(sprintf("%s_%s", INDEX[[1L]], nm))
  delay_state <- delay_name(sprintf("%s_%s", STATE, nm))
  delay_dim <- array_dim_name(delay_idx)
  true_time <- sprintf("odin_true_%s", time_name)

  ## If there are any arrays here we'll need to organise offsets.
  ## Rather than store the full offset vector I'll do this one by hand
  ## I think and let the compiler take care of it?  Getting this right
  ## will require a good test.  It's possible that this can be done
  ## with an accumulating variable as we'll need to get lengths here
  ## anyway.
  st <- STAGES[if (obj$info$has_array) obj$info$dim_stage else STAGE_CONSTANT]

  obj$add_element(delay_idx, "int", 1L)
  obj$add_element(delay_state, "double", 1L)

  obj[[st]]$add("%s = %s;", obj$rewrite(delay_dim), delay_len_tot, name=nm)
  if (st == "user") { ## NOTE: duplicated from odin_generate1_dim()
    obj$constant$add("%s = NULL;", obj$rewrite(delay_idx))
    obj$constant$add("%s = NULL;", obj$rewrite(delay_state))
    obj$user$add("Free(%s);", obj$rewrite(delay_idx))
    obj$user$add("Free(%s);", obj$rewrite(delay_state))
  }
  obj[[st]]$add("%s = (int*) Calloc(%s, int);",
                obj$rewrite(delay_idx), obj$rewrite(delay_dim), name=nm)
  obj[[st]]$add("%s = (double*) Calloc(%s, double);",
                obj$rewrite(delay_state), obj$rewrite(delay_dim), name=nm)
  obj$free$add("Free(%s);", obj$rewrite(delay_idx))
  obj$free$add("Free(%s);", obj$rewrite(delay_state))

  ## Fill in the instructions for deSolve as to which variables we
  ## need delays for.  This is pretty hairy in the case of variables
  ## because we need to work across two sets of offsets that don't
  ## necessarily match up.  Note that the non-array things go in here
  ## before the array things.

  ## TODO: I'm not actually 100% sure about this; mostly why are both
  ## $order and $name in here?
  if (x$delay$expr$n > 0L) {
    i <- match(x$delay$expr$order, obj$variable_info$order)
    delay_var_offset <- obj$variable_info$offset_use[i]
    obj[[st]]$add("{", name=nm)
    obj[[st]]$add("  // delay block for %s", nm, name=nm)
    obj[[st]]$add("  int j = 0;", name=nm)
    for (i in seq_len(x$delay$expr$n)) {
      if (x$delay$expr$is_array[[i]]) {
        obj[[st]]$add("  for (int i = 0, k = %s; i < %s; ++i) {",
                      obj$rewrite(delay_var_offset[[i]]),
                      obj$rewrite(array_dim_name(x$delay$expr$names[[i]])),
                      name=nm)
        obj[[st]]$add("    %s[j++] = k++;", obj$rewrite(delay_idx), name=nm)
        obj[[st]]$add("  }", name=nm)
      } else {
        obj[[st]]$add("  %s[j++] = %s;", obj$rewrite(delay_idx),
                      obj$rewrite(delay_var_offset[[i]]), name=nm)
      }
    }
    obj[[st]]$add("}", name=nm)
  }

  ## TODO: this is an obvious point to break this function; bits for
  ## the preparation and bits for the execution.  I'll get that done
  ## as soon as this approximately works.

  ## Next, prepare output variables so we can push them up out of scope:
  ret <- collector()
  if (x$lhs$type == "symbol") {
    ret$add("double %s;", nm)
  }
  ret$add("{")
  ret$add("  // delay block for %s", nm)

  has_default <- !is.null(x$rhs$value_default)

  ## 4. Pull things out of the lag value, but only if time is past
  ## where the lag is OK to work with.  That's going to look like:
  if (x$delay$expr$n > 0) {
    declare_vars <- sprintf("double %s;",
                            paste0(ifelse(x$delay$expr$is_array, "*", ""),
                                   x$delay$expr$order, collapse=", "))
    if (!has_default) {
      ret$add(indent(declare_vars, 2))
    }
  }
  ret$add("  const %s %s = %s;", time_type, true_time, time_name)
  ret$add("  const %s %s = %s - %s;",
          time_type, time_name, true_time, obj$rewrite(x$delay$time))

  ## NOTE: the paths with- and without delays are quite different; we
  ## only unpack anything in the case where there is not a default
  ## because otherwise we'd just use the top-level code.
  if (has_default || x$delay$expr$n > 0) {
    ret$add("  if (%s <= %s) {",
            time_name, obj$rewrite(initial_name(time_name)))
    if (has_default) {
      ## NOTE/TODO: if this uses any time-dependent arrays, we have
      ## problems unless I enforce that this must be resolvable in the
      ## graph based on default being treated as a normal expression
      ## (that seems reasonable).  In that case we're just going to
      ## rewrite the _final_ expression and be done with it.  That's
      ## really simple at least and removes all the faff.  So we still
      ## need to inject the appropriate bits there.
      if (x$lhs$type == "array") {
        xd <- x
        xd$rhs <- x$delay$default
        xd$rhs$value <- list(xd$rhs$value)
        ret$add(indent(odin_generate1_array_expr(xd, obj), 4))
      } else {
        ret$add(indent("%s = %s;", 4), nm, obj$rewrite(x$delay$default$value))
      }
    } else if (x$delay$expr$n > 0) {
      ret$add("    %s = %s;",
              x$delay$expr$order, vcapply(initial_name(x$delay$expr$order),
                                          obj$rewrite, USE.NAMES=FALSE))
    }
    ret$add("  } else {")
    if (x$delay$expr$n > 0 && has_default) {
      ret$add(indent(declare_vars, 4))
    }
    lagvalue <-  sprintf(indent("lagvalue_%%s(%s, %s, %s, %s);", 6),
                         time_name,
                         obj$rewrite(delay_idx),
                         obj$rewrite(delay_dim),
                         obj$rewrite(delay_state))
    ret$add("    if (%s) {", obj$rewrite("odin_use_dde"))
    ret$add(lagvalue, "dde")
    ret$add("    } else {")
    ret$add(lagvalue, "ds")
    ret$add("    }")

    ## There are 4 possibilities here;
    delay_access <- function(i) {
      ai <- x$delay$expr$access[[i]]
      if (!x$delay$expr$is_array[[i]]) {
        sprintf("%s[%s]", obj$rewrite(delay_state), ai)
      } else if (is.null(ai)) {
        obj$rewrite(delay_state)
      } else if (is.numeric(ai)) {
        obj$rewrite(call("+", as.name(delay_state), ai))
      } else {
        obj$rewrite(ai)
      }
    }
    ret$add(indent("%s = %s;", 4),
            x$delay$expr$order,
            vcapply(seq_len(x$delay$expr$n), delay_access))

    ## This is where the two routes (with- and without-default) start
    ## diverging more markedly.
    if (!has_default) {
      ret$add("  }")
    }
  }
  nindent <- if (has_default) 4L else 2L

  ## Then we'll organise that whenever we hit a variable that is used
  ## in a delay statement we'll add it in here in the appropriate
  ## order.
  ##
  ## This one is nasty:
  if (any(x$delay$expr$deps_is_array)) {
    subs <- names_if(x$delay$expr$deps_is_array)
    subs <- setNames(lapply(delay_name(subs), as.name), subs)
    tr <- function(x) {
      if (x$name %in% names(subs)) {
        x$name <- as.character(subs[[x$name]])
      }
      if (isTRUE(x$rhs$delay)) {
        ## This one is the actual target (last step in the process).
        x$rhs$value <- list(substitute_(x$rhs$value_expr, subs))
      } else {
        ## This one is an array used to generate the target.
        x$rhs$value <- lapply(x$rhs$value, substitute_, subs)
      }
      x
    }
  } else {
    if (x$lhs$type == "array") {
      x$rhs$value <- list(x$rhs$value_expr)
    }
    tr <- identity
  }

  ## Here, identify and rewrite the arrays from the equation.
  for (nm_dep in x$delay$expr$deps) {
    if (x$delay$expr$deps_is_array[[nm_dep]]) {
      ret$add(indent(
                 odin_generate1_array_expr(tr(eqs[[nm_dep]]), obj), nindent),
                 name=nm)
    } else {
      ret$add(indent("double %s = %s;", nindent),
                    nm_dep, obj$rewrite(tr(eqs[[nm_dep]])$rhs$value),
                    name=nm)
    }
  }
  if (x$lhs$type == "array") {
    ret$add(indent(odin_generate1_array_expr(tr(x), obj), nindent),
                  name=nm)
  } else {
    ret$add(indent("%s = %s;", nindent),
                  nm, obj$rewrite(tr(x)$rhs$value_expr))
  }

  if (has_default) {
    ret$add("  }")
  }
  ret$add("}")

  obj$time$add(ret$get(), name = nm)
}

## This is heaps easier than the ode version because we do not need to
## reconstruct the graph for delay variables that are not themselves
## variables (because we are guaranteed to hit every time in order).
## So we can just push things onto the ring and pop them off again.
odin_generate1_delay_discrete <- function(x, obj, eqs) {
  nm <- x$name
  time_name <- STEP
  time_type <- "int"
  ring_head <- sprintf("%s_head", RING) # must match up with generate2

  has_default <- !is.null(x$rhs$value_default)

  ## This is a bit different to usual I think.  We're going to need to
  ## set up initial conditions for delayed variables; generally we
  ## need to assume that they're stochastic so we want to evaluate
  ## them only once!
  ##
  ## Getting that right requires that we can access the correct point
  ## in the graph.
  if (has_default) {
    ## This requires some serious thinking, possibly not that much
    ## actual work though.
    stop("FIXME")
  }

  ret <- collector()
  offset <- obj$discrete_delay_info$offset_use[[offset_name(nm)]]

  ret$add("// calculation for current value of %s", nm)
  ret$add("{")
  if (x$lhs$type == "array") {
    ## OK, this is cool; this just does not work right because we need
    ## to rewrite 'x' a bit to get this all faked together nicely.

    ## TODO: I think that it's quite likely that (in general) this
    ## will not work with multipart expressions, but that's OK.  That
    ## would look something like:
    ##
    ##   x[1] <- delay(y[1])
    ##   x[2] <- delay(y[2])
    ##
    ## but I don't think that's tested anywhere yet.
    xd <- x
    xd$rhs <- list(value = list(x$rhs$value_expr))
    ret$add("  double * %s = %s + %s;", nm, ring_head, obj$rewrite(offset))
    ret$add(indent(odin_generate1_array_expr(xd, obj), 2))
  } else {
    ret$add("  %s[%s] = %s;", ring_head, obj$rewrite(offset),
            obj$rewrite(x$rhs$value_expr))
  }
  ret$add("}")

  time_offset_name <- sprintf("%s_offset", time_name)
  ret$add("// delayed value of %s", nm)
  ## Lift the variables up out of the next scope
  if (x$lhs$type == "symbol") {
    ret$add("double %s;", nm)
  } else {
    ret$add("const double * %s;", nm)
  }
  ret$add("{")

  ## NOTE could simplify this in the case where x$delay$time is symbol
  ## or atomic, though the compiler should do that for us anyway.
  ret$add("  %s %s = %s;",
          time_type, time_offset_name, obj$rewrite(x$delay$time))

  ret$add("  if ((int)%s - %s < %s) {",
    time_name, time_offset_name, obj$rewrite(initial_name(time_name)))
  ret$add("    %s = %s - %s;",
          time_offset_name, time_name, obj$rewrite(initial_name(time_name)))
  ret$add("  }")
  ret$add(
    "  const double * head = (double*) ring_buffer_head_offset(%s, %s - 1);",
    obj$rewrite(RING), time_offset_name)
  ret$add("  if (%s == 0) {", time_offset_name);
  ret$add("    head = %s;", ring_head)
  ## TODO: Next two lines come out once this is working properly:
  ret$add("  } else if (head == NULL) {")
  ret$add('    Rf_error("This is an odin bug");')
  ret$add("  }")
  if (x$lhs$type == "array") {
    ret$add("  %s = head + %s;", nm, obj$rewrite(offset));
  } else {
    ret$add("  %s = head[%s];", nm, obj$rewrite(offset))
  }
  ret$add("}")
  ## Done here:
  obj$time$add(ret$get(), name = nm)
}

odin_generate1_interpolate <- function(x, obj) {
  nm <- x$name
  tmp <- x$rhs$value
  nd <- tmp$nd
  ny <- tmp$ny
  nt <- tmp$nt
  nm_t <- tmp$t
  nm_y <- tmp$y
  interpolation_type <- tmp$type
  dest <- tmp$name

  obj$interpolate$add(list(interpolation_type=interpolation_type, t=nm_t))
  obj$add_element(dest, "void")

  ## TODO: throughout here; consider looking at the actual definitions
  ## as it may be possible to determine that the conditional will
  ## always be true.  The compiler should sort that out for us though,
  ## though it may give warnings.
  if (nd == 0L) {
    obj$add_element(nm, "double")
    obj$user$add('odin_interpolate_check(%s, %s, 0, "%s", "%s");',
                 obj$rewrite(nt), obj$rewrite(array_dim_name(nm_y)), nm_y, nm)
    n_target <- 1L
  } else {
    n_target <- array_dim_name(nm)
    for (i in seq_len(nd + 1)) {
      if (i == 1L) {
        dim_target <- array_dim_name(nm_t)
      } else {
        dim_target <- array_dim_name(nm, if (nd == 1) NULL else i - 1)
      }
      obj$user$add('odin_interpolate_check(%s, %s, %d, "%s", "%s");',
                   obj$rewrite(dim_target),
                   obj$rewrite(array_dim_name(nm_y, i)), i, nm_y, nm)
    }
  }

  ## TODO: need to check that the dimensions of the arrays are OK.
  ## That's actually not totally straightforward.  It goes in the user
  ## bit right here before free/alloc though.
  ##
  ## TODO: Right before running (so init stage) we need to check that
  ## the times span appropriately.
  ##
  ## TODO: At some point we'll need to specify some critical values so
  ## that the integrator doesn't struggle with things having to change
  ## radically.
  obj$user$add('cinterpolate_free(%s);', obj$rewrite(dest))
  obj$user$add('%s = cinterpolate_alloc("%s", %s, %s, %s, %s);',
               obj$rewrite(dest), interpolation_type,
               obj$rewrite(nt), obj$rewrite(n_target),
               obj$rewrite(nm_t), obj$rewrite(nm_y))
  obj$free$add('cinterpolate_free(%s);', obj$rewrite(dest))

  ## TODO: These are going to do tricky things with time when delayed.
  ##
  ## TODO: don't have error handling done here yet - it's not totally
  ## clear what we should do.  The safest thing is going to be to
  ## throw an error and just bail.  After that the next best thing to
  ## do is not go further than the end?
  target <- sprintf(if (x$lhs$type == "array") "%s" else "&(%s)",
                    obj$rewrite(nm))
  time_name <- if (obj$info$discrete) STEP else TIME
  obj$time$add("cinterpolate_eval(%s, %s, %s);",
               time_name, obj$rewrite(dest), target,
               name = nm)
}

odin_generate1_dim_array_dimensions <- function(x, rewrite) {
  ret <- collector()

  if (x$nd == 1) {
    stop("[odin bug]") # nocov
  }

  nm <- x$name
  nm_target <- x$lhs$name_target
  nm_i <- vcapply(seq_len(x$nd),
                  function(i) rewrite(array_dim_name(nm_target, i)))
  if (x$nd >= 3L) {
    for (j in 3:x$nd) {
      k <- seq_len(j - 1)
      dn <- rewrite(array_dim_name(nm_target, paste(k, collapse="")))
      ret$add("%s = %s;", dn, paste(nm_i[k], collapse=" * "))
    }
  }
  ret$add("%s = %s;", rewrite(nm), paste(nm_i, collapse=" * "))

  ret$get()
}
