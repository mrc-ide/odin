## TODO: Disallow '<base>_' as a name; otherwise potential for
## collision, so probably set that in the DSL rather than here.
odin_generate1 <- function(dat) {
  obj <- odin_generate1_object(dat)

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
  obj$add_element("odin_use_dde", "int")
  if (obj$info$has_delay) {
    obj$add_element(initial_name(TIME), "double")
  }
  odin_generate1_library(obj, dat$eqs)

  ## The main loop over all equations:
  odin_generate1_loop(obj, dat$eqs)

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
odin_generate1_object <- function(dat) {
  base <- dat$config$base
  self <- list(base=base)

  self$info <- list(base=base,
                    ## Core traits
                    has_delay=dat$info$has_delay,
                    has_output=dat$info$has_output,
                    has_interpolate=dat$info$has_interpolate,
                    has_array=dat$info$has_array,
                    ## Initialisation stages
                    initial_stage=dat$initial_info$stage,
                    dim_stage=dat$dim_stage,
                    ## User variables
                    user=dat$user_default)

  self$variable_info <- dat$variable_info
  self$output_info <- dat$output_info
  self$initial_info <- dat$initial_info

  self$name_pars <- sprintf("%s_p", base)
  self$type_pars <- sprintf("%s_pars", base)

  ## The major stages; we'll collect things into these as the main
  ## loop runs.
  self$constant <- collector_named()
  self$user <- collector_named()
  self$time <- collector_named(TRUE)
  self$output <- collector_named(TRUE)

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

  self$add_element <- function(name, type, array=0) {
    if (array > 0L) {
      name_dim <- array_dim_name(name, use=FALSE)
      if (nzchar(name_dim)) {
        Recall(name_dim, "int", FALSE)
        if (array > 1L) {
          for (i in seq_len(array)) {
            Recall(array_dim_name(name, i, use=FALSE), "int")
          }
          if (array == 3L) {
            Recall(array_dim_name(name, "12", use=FALSE), "int")
          }
        }
      }
    }
    self$types$add(list(name=name, type=type, array=array))
    self$lookup$add(name)
  }

  ## Custom functions, defined in .c files:
  self$custom <- dat$config$include
  custom_functions <- names(self$custom$declarations)

  ## Rewrite based on all the above; only lookup is modified as we go.
  self$rewrite <- function(x) {
    rewrite_c(x, self$name_pars, self$lookup$get(), INDEX, custom_functions)
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
  ## TODO: this may miss things in delay rhs?
  used_functions <- unique(unlist(lapply(eqs, function(x) x$depends$functions)))
  if ("sum" %in% used_functions) {
    obj$library_fns$add("odin_sum1")
    obj$library_fns$add("odin_sum2")
    obj$library_fns$add("odin_sum3")
  }
  if ("%%" %in% used_functions) {
    obj$library_fns$add("fmodr")
  }

  ## Support for differential equations:
  if (obj$info$has_delay) {
    obj$library_fns$add("lagvalue_dde")
    obj$library_fns$add("lagvalue_ds")
  }
}

odin_generate1_dim <- function(x, obj) {
  nm <- x$name
  nm_target <- x$lhs$name_target
  is_var <- nm_target %in% obj$variable_info$order
  nm_s <- if (is_var) initial_name(nm_target) else nm_target
  st <- STAGES[[x$stage]]

  obj$add_element(nm_s, "double", x$nd)
  if (x$stage == STAGE_USER) {
    obj$constant$add("%s = NULL;", obj$rewrite(nm_s))
    obj$user$add("Free(%s);", obj$rewrite(nm_s))
  } else if (x$stage > STAGE_USER) {
    stop("This should never happen! [odin bug]") # nocov
  }

  if (x$nd > 1L) {
    obj$library_fns$add(sprintf("odin_set_dim%d", x$nd))
  }

  if (isTRUE(x$rhs$user)) {
    ## Here, we'll need to a little extra work; get the value, check
    ## length, copy out integers.
    fn <- sprintf("get_user_array_dim%d", x$nd)
    obj$library_fns$add(fn)
    ## We really need to do this in a scoped block I think as we need
    ## to set a few things all at once.
    obj[[st]]$add("{")

    if (x$nd > 1L) {
      nm_i <- vcapply(seq_len(x$nd),
                      function(i) obj$rewrite(array_dim_name(nm_target, i)))
    } else {
      nm_i <- obj$rewrite(nm)
    }

    obj[[st]]$add('  double *tmp = %s(%s, "%s", %s);',
                  fn, USER, nm_s, paste0("&", nm_i, collapse=", "))
    ## TODO: This duplicates the code below for computing compound
    ## dimensions, but until I get test cases in that's probably the
    ## simplest for now (note it differs in indent though).
    if (x$nd > 1L) {
      if (x$nd == 3L) {
        obj[[st]]$add("  %s = %s;",
                      obj$rewrite(array_dim_name(nm_target, "12")),
                      paste(nm_i[1:2], collapse=" * "))
      }
      obj[[st]]$add("  %s = %s;", obj$rewrite(nm), paste(nm_i, collapse=" * "))
    }
    obj[[st]]$add("  %s = (double*) Calloc(%s, double);",
                  obj$rewrite(nm_s), obj$rewrite(nm))
    obj[[st]]$add("  memcpy(%s, tmp, %s * sizeof(double));",
                  obj$rewrite(nm_s), obj$rewrite(nm))
    obj[[st]]$add("}")
  } else {
    if (x$nd > 1L) {
      size <- as.list(x$rhs$value[-1L])
      nm_i <- vcapply(seq_len(x$nd),
                      function(i) obj$rewrite(array_dim_name(nm_target, i)))
      for (i in seq_len(x$nd)) {
        obj[[st]]$add("%s = %s;", nm_i[[i]], obj$rewrite(size[[i]]))
      }
      ## Little extra work for the 3d case.  If we were allowing
      ## arbitrary matrices here this would be heaps more complicated
      ## but we only need the special case here.
      if (x$nd == 3L) {
        obj[[st]]$add("%s = %s;",
                      obj$rewrite(array_dim_name(nm_target, "12")),
                      paste(nm_i[1:2], collapse=" * "))
      }
      obj[[st]]$add("%s = %s;", obj$rewrite(nm), paste(nm_i, collapse=" * "))
    } else {
      obj[[st]]$add("%s = %s;", obj$rewrite(nm), obj$rewrite(x$rhs$value))
    }
    obj[[st]]$add("%s = (double*) Calloc(%s, double);",
                  obj$rewrite(nm_s), obj$rewrite(nm))
  }
  obj$free$add("Free(%s);", obj$rewrite(nm_s))

  if (isTRUE(x$used_in_delay)) {
    ## Storage for array components that are used within delay
    ## expressions.  These will need to be extracted twice (once for
    ## "now", once in a delay expression) so we need to create a bit
    ## of extra space for them.
    nm_delay <- delay_name(nm_target)
    ## TODO: I'm not sure about the 1 on the end here; why is this not
    ## the correct nd here?  Just to avoid writing out all the
    ## subindexing?
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
    nm_offset <- paste0("offset_", nm_target)
    obj$add_element(nm_offset, "int")
    i <- match(nm_target, obj$variable_info$order)
    offset <- obj$variable_info$offset[[i]]
    obj[[st]]$add("%s = %s;", obj$rewrite(nm_offset), obj$rewrite(offset))
  }
}

odin_generate1_symbol <- function(x, obj) {
  st <- STAGES[[x$stage]]
  nm <- x$name
  type <- x$lhs$data_type

  is_initial <- identical(x$lhs$special, "initial")
  st <- if (is_initial) "initial" else STAGES[[x$stage]]
  if (x$stage < STAGE_TIME || is_initial) {
    obj$add_element(nm, type)
  }

  if (isTRUE(x$rhs$user)) {
    if (isTRUE(x$rhs$default)) {
      default <- obj$rewrite(x$rhs$value)
    } else {
      default <- if (type == "int") "NA_INTEGER" else "NA_REAL"
    }
    obj$constant$add("%s = %s;", obj$rewrite(nm), default)
  }

  obj[[st]]$add(odin_generate1_symbol_expr(x, obj), name=nm)
}

odin_generate1_symbol_expr <- function(x, obj) {
  nm <- x$name
  type <- x$lhs$data_type

  if (isTRUE(x$rhs$user)) {
    get_user <- sprintf("get_user_%s", type)
    obj$library_fns$add(get_user)
    value <- sprintf("%s(%s, \"%s\", %s)", get_user, USER, nm, obj$rewrite(nm))
  } else {
    value <- obj$rewrite(x$rhs$value)
  }

  is_initial <- identical(x$lhs$special, "initial")
  if (x$stage < STAGE_TIME || is_initial) {
    ret <- sprintf("%s = %s;", obj$rewrite(nm), value)
  } else if (identical(x$lhs$special, "deriv")) {
    ## NOTE: offset guaranteed to be OK, but should probably rewrite?
    i <- match(x$lhs$name_target, obj$variable_info$order)
    offset <- obj$rewrite(obj$variable_info$offset_use[[i]])
    ret <- sprintf("%s[%s] = %s;", DSTATEDT, offset, value)
  } else if (identical(x$lhs$special, "output")) {
    i <- match(x$lhs$name_target, obj$output_info$order)
    offset <- obj$rewrite(obj$output_info$offset_use[[i]])
    ret <- sprintf("%s[%s] = %s;", OUTPUT, offset, value)
  } else {
    ret <- sprintf("%s %s = %s;", type, nm, value)
  }
  ret
}

odin_generate1_array <- function(x, obj, eqs) {
  st <- STAGES[[x$stage]]
  nm <- x$name

  if (isTRUE(x$rhs$user)) {
    dim <- eqs[[array_dim_name(x$name)]]
    if (isTRUE(dim$rhs$user)) {
      ## All done already while establishing dim
      return()
    }
    nd <- dim$nd
    fn <- sprintf("get_user_array%d", nd)
    obj$library_fns$add(fn)

    if (nd == 1) {
      dn <- obj$rewrite(dim$name)
    } else {
      dn <- paste(vcapply(seq_len(nd), function(i)
        obj$rewrite(array_dim_name(x$name, i)), USE.NAMES=FALSE), collapse=", ")
    }
    obj[[st]]$add('%s(%s, "%s", %s, %s);',
                  fn, USER, nm, dn, obj$rewrite(x$name), name=nm)
  } else if (isTRUE(x$rhs$output_self)) {
    obj[[st]]$add("memcpy(%s, %s, %s * sizeof(double));",
                  obj$rewrite(x$name), obj$rewrite(x$lhs$name_target),
                  obj$rewrite(array_dim_name(x$lhs$name_target)),
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
    target <- obj$rewrite(as.call(c(quote(`[`), as.symbol(x$name), target)))
    value <- obj$rewrite(x$rhs$value[[j]])
    ret$add("%s%s = %s;", indent, target, value)

    while (nzchar(indent)) {
      indent <- substr(indent, 3L, nchar(indent))
      ret$add("%s}", indent)
    }
  }
  ret$get()
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
odin_generate1_delay <- function(x, obj, eqs) {
  nm <- x$name
  delay_len <- length(x$delay$extract)
  delay_is_array <- x$delay$is_array
  ## This one is nasty:
  deps_is_array <- x$delay$deps_is_array

  delay_size <- vcapply(x$delay$size, obj$rewrite)
  ## Need to compute total array size here, with the 3 options of all
  ## array, no array or some array:
  if (all(delay_is_array)) {
    delay_size_tot <- paste(delay_size, collapse=" + ")
  } else if (any(delay_is_array)) {
    delay_size_tot <-
      paste(c(sum(!delay_is_array), delay_size[delay_is_array]),
            collapse=" + ")
  } else {
    delay_size_tot <- delay_len
  }

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
  delay_idx <- delay_name(sprintf("%s_%s", INDEX[[1L]], nm))
  delay_state <- delay_name(sprintf("%s_%s", STATE, nm))
  delay_dim <- array_dim_name(delay_idx)
  delay_time <- delay_name(TIME)

  ## If there are any arrays here we'll need to organise offsets.
  ## Rather than store the full offset vector I'll do this one by hand
  ## I think and let the compiler take care of it?  Getting this right
  ## will require a good test.  It's possible that this can be done
  ## with an accumulating variable as we'll need to get lengths here
  ## anyway.
  st <- STAGES[if (obj$info$has_array) obj$info$dim_stage else STAGE_CONSTANT]

  obj$add_element(delay_idx, "int", 1L)
  obj$add_element(delay_state, "double", 1L)

  obj[[st]]$add("%s = %s;", obj$rewrite(delay_dim), delay_size_tot, name=nm)
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
  i <- match(x$delay$extract, obj$variable_info$order)
  delay_var_offset <- obj$variable_info$offset_use[i]
  obj[[st]]$add("{", name=nm)
  obj[[st]]$add("  // delay block for %s", nm, name=nm)
  obj[[st]]$add("  int j = 0;", name=nm)
  for (i in seq_along(delay_var_offset)) {
    if (x$delay$is_array[[i]]) {
      obj[[st]]$add("  for (int i = 0, k = %s; i < %s; ++i) {",
                    obj$rewrite(delay_var_offset[[i]]),
                    obj$rewrite(array_dim_name(x$delay$extract[[i]])),
                    name=nm)
      obj[[st]]$add("    %s[j++] = k++;", obj$rewrite(delay_idx), name=nm)
      obj[[st]]$add("  }", name=nm)
    } else {
      obj[[st]]$add("  %s[j++] = %s;", obj$rewrite(delay_idx),
                    obj$rewrite(delay_var_offset[[i]]), name=nm)
    }
  }
  obj[[st]]$add("}", name=nm)

  ## Next, prepare output variables so we can push them up out of scope:
  st <- STAGES[STAGE_TIME]
  if (x$lhs$type == "symbol") {
    obj[[st]]$add("double %s;", nm, name=nm)
  }
  obj[[st]]$add("{", name=nm)

  ## 4. Pull things out of the lag value, but only if time is past
  ## where the lag is OK to work with.  That's going to look like:
  obj[[st]]$add("  double %s;",
                paste0(ifelse(x$delay$is_array, "*", ""),
                       x$delay$extract, collapse=", "), name=nm)

  ## Next, we need to compute offsets.  This is annoying because it
  ## duplicates code elsewhere, but life goes on.  I'm running this
  ## one a bit differently though, in the hope that not too many ways.
  ##
  ## Here, we need, for the array case, to swap out the delay_state in
  ## place of the last array.  If I do that always it's less checking,
  ## actually.  This will always be of the form X + dim(X) so that's
  ## nice.
  compute_offset <- function(i) {
    if (identical(x$delay$offset[[i]], 0L) && x$delay$is_array[[i]]) {
      obj$rewrite(delay_state)
    } else {
      fmt <- if (x$delay$is_array[[i]]) "%s + %s" else "%s[%s]"
      if (i > 1L && x$delay$is_array[[i - 1L]]) {
        base <- x$delay$extract[[i - 1L]]
      } else {
        base <- obj$rewrite(delay_state)
      }
      sprintf(fmt, base, obj$rewrite(x$delay$offset[[i]]))
    }
  }
  delay_access <- vcapply(seq_along(x$delay$offset), compute_offset)

  ## TODO: if time is used in the time calculation it will need
  ## rewriting.  But I believe that parse prohibits that in the
  ## meantime.
  obj[[st]]$add("  const double %s = %s - %s;",
                delay_time, TIME, obj$rewrite(x$delay$time), name=nm)
  obj[[st]]$add("  if (%s <= %s) {",
                delay_time, obj$rewrite(initial_name(TIME)), name=nm)
  obj[[st]]$add("    %s = %s;",
                x$delay$extract,
                vcapply(initial_name(x$delay$extract),
                        obj$rewrite, USE.NAMES=FALSE), name=nm)
  obj[[st]]$add("  } else {", name=nm)
  lagvalue <-  sprintf("      lagvalue_%%s(%s, %s, %s, %s);",
                       delay_time,
                       obj$rewrite(delay_idx),
                       obj$rewrite(delay_dim),
                       obj$rewrite(delay_state))
  obj[[st]]$add("    if (%s) {", obj$rewrite("odin_use_dde"), name=nm)
  obj[[st]]$add(lagvalue, "dde", name=nm)
  obj[[st]]$add("    } else {", name=nm)
  obj[[st]]$add(lagvalue, "ds", name=nm)
  obj[[st]]$add("    }", name=nm)
  obj[[st]]$add("    %s = %s;", x$delay$extract, delay_access, name=nm)
  obj[[st]]$add("  }", name=nm)

  ## Then we'll organise that whenever we hit a variable that is used
  ## in a delay statement we'll add it in here in the appropriate
  ## order.
  if (any(x$delay$deps_is_array)) {
    subs <- names_if(x$delay$deps_is_array)
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
    tr <- identity
  }

  ## Here, identify and rewrite the arrays from the equation.
  for (nm_dep in x$delay$deps) {
    if (deps_is_array[[nm_dep]]) {
      obj[[st]]$add(indent(
                 odin_generate1_array_expr(tr(eqs[[nm_dep]]), obj), 2),
                 name=nm)
    } else {
      obj[[st]]$add("  double %s = %s;",
                    nm_dep, obj$rewrite(tr(eqs[[nm_dep]])$rhs$value),
                    name=nm)
    }
  }
  if (x$lhs$type == "array") {
    obj[[st]]$add(indent(odin_generate1_array_expr(tr(x), obj), 2), name=nm)
  } else {
    obj[[st]]$add("  %s = %s;", nm, obj$rewrite(tr(x)$rhs$value_expr),
                  name=nm)
  }
  obj[[st]]$add("}", name=nm)
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
  obj$add_element(dest, "interpolate_data")

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
  obj$user$add('interpolate_free(%s);', obj$rewrite(dest))
  obj$user$add('%s = interpolate_alloc(%s, %s, %s, %s, %s);',
               obj$rewrite(dest), toupper(interpolation_type),
               obj$rewrite(nt), obj$rewrite(n_target),
               obj$rewrite(nm_t), obj$rewrite(nm_y))
  obj$free$add('interpolate_free(%s);', obj$rewrite(dest))

  ## TODO: These are going to do tricky things with time when delayed.
  ##
  ## TODO: don't have error handling done here yet - it's not totally
  ## clear what we should do.  The safest thing is going to be to
  ## throw an error and just bail.  After that the next best thing to
  ## do is not go further than the end?
  target <- sprintf(if (x$lhs$type == "array") "%s" else "&(%s)",
                    obj$rewrite(nm))
  obj$time$add("interpolate_%s_run(%s, %s, %s);",
               interpolation_type, TIME, obj$rewrite(dest), target,
               name=nm)
}
