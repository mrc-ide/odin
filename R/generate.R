## The issue here is that we care about types a lot; so for the

odin_generate <- function(dat, base="odin", dest=tempfile()) {
  ##dat$generated <- list(header=odin_generate_header(base))
  data <- odin_generate_loop(dat, base)
  txt <- wr(read_file(system.file("template.whisker", package="odin")), data)
  writeLines(txt, dest)
  dest
}

odin_generate_header <- function(base) {
  template <- read_file(system.file("header.whisker", package="odin"))
  wr(template, list(prefix=base))
}

## TODO: Disallow base_ as a name; otherwise potential for collision,
## so probably pull that from the DSL?
odin_generate_loop <- function(dat, base) {
  types <- collector()
  init <- collector()
  user <- collector()
  deriv <- collector()
  free <- collector()
  copy <- collector()
  order <- collector()

  ## TODO: Going to need to change some of the name here about as the
  ## names no longer reflect things correctly.
  ##   constant -> create(?)
  ##   user -> initialise
  ##   time ok
  ## but there are already 5 names here and they don't match up with
  ## the STAGE names!
  res <- list(constant=init, user=user, time=deriv)[STAGES]

  name_pars <- sprintf("%s_p", base)
  type_pars <- sprintf("%s_pars", base)

  contents <- collector()
  contents$i <- 0L
  contents_add <- function(name, type, array) {
    contents$add("SET_STRING_ELT(%s_names, %d, mkChar(\"%s\"));",
                 STATE, contents$i, name)
    if (array) {
      rtype <- if (type == "int") "INTSXP" else "REALSXP"
      raccess <- if (type == "int") "INTEGER" else "REAL"
      if (grepl("^initial_", name)) {
        name_dim <- sub("^initial_", "dim_", name)
      } else {
        name_dim <- sprintf("dim_%s", name)
      }
      contents$add("SET_VECTOR_ELT(%s, %d, allocVector(%s, %s->%s));",
                   STATE, contents$i, rtype, name_pars, name_dim)
      contents$add(
        "memcpy(%s(VECTOR_ELT(%s, %d)), %s->%s, %s->%s * sizeof(%s));",
        raccess, STATE, contents$i, name_pars, name, name_pars, name_dim, type)
    } else {
      type2 <- if (type == "int") "Integer" else "Real"
      contents$add("SET_VECTOR_ELT(%s, %d, Scalar%s(%s->%s));",
                   STATE, contents$i, type2, name_pars, name)
    }
    contents$i <<- contents$i + 1L
  }
  type_add <- function(name, type, array) {
    if (array) {
      types$add("%s *%s;", type, name)
    } else {
      types$add("%s %s;", type, name)
    }
    contents_add(name, type, array)
  }

  ## Set up initial time so we can refer to it later.  Not all models
  ## make use of this, but it seems worth adding (and doesn't take
  ## that much space).  Delay models will make extensive use of this.
  type_add(paste0("initial_", TIME), "double", FALSE)
  res[[STAGE_CONSTANT]]$add("%s->initial_%s = NA_REAL;", name_pars, TIME)
  res[[STAGE_INITIAL]]$add("%s->initial_%s = %s;", name_pars, TIME, TIME)

  ## We'll need to find all variables within the struct *except* for
  ## the derivatives.  This could be extended to include non-array
  ## variables computed during the derivative calculations, but these
  ## will need modification below to skip the declaration in the
  ## struct and move it into the stack allocation within `user`
  ## (TODO).
  lookup <- vcapply(dat$eqs, "[[", "name")
  lookup <- lookup[viapply(dat$eqs, "[[", "stage") < STAGE_TIME]
  lookup <- c(
    lookup,
    unlist(dat$variable_order$offset_use[dat$variable_order$offset_is_var],
           use.names=FALSE),
    if (dat$variable_order$total_is_var) dat$variable_order$total_use)
  ## TODO: Need to add information for all arrays that have more than
  ## one dimension because we'll need to copy things like dim_<x>_1
  ## and dim_<x>_12 over too.

  rewrite <- function(x) {
    rewrite_c(x, name_pars, lookup, INDEX)
  }

  nms <- vcapply(dat$eqs, function(x) x$lhs$name)
  is_array <- vlapply(dat$eqs, function(x) x$lhs$type == "array")

  delay <- vector("list", length(dat$delay))

  if (!is.null(dat$delay)) { # delay model
    ## NOTE: In the pointer creation we might need to return a little
    ## additional information about things like being a delay model or
    ## not.  Return a list?  Affects the template mostly.
    for (i in seq_along(dat$delay)) {
      delay_i <- dat$delay[[i]]
      delay_c <- collector()

      ## TODO: This can be relaxed soon, just requires more book-keeping.
      if (any(dat$variable_order$is_array[delay_i$extract])) {
        stop("Delay arrays not supported")
      }
      if (any(is_array[match(delay_i$order, nms)])) {
        stop("Delay dependency arrays not supported")
      }

      ## Then, we need to pull the variables out!  To do that:
      ## Number of variables:
      ## type_add(sprintf("delay_%d_m", i), "int", FALSE)
      ## ## This one is always constant:
      ## res[[STAGE_CONSTANT]]$add("%s->delay_%d_m = %d;",
      ##   name_pars, i, delay_i_n)
      ##
      ## TODO: This one varies with things like the length of arrays;
      ## we'll be looking for
      ##   tmp <- names(which(dat$variable_order$is_array[delay_i$extract]))
      ##   paste(sprintf("dim_%s", tmp), collapse=" + ")
      ## added onto
      ##   length(delay_i$extract) -
      ##     sum(dat$variable_order$is_array[delay_i$extract])
      ##
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
      ## TODO: Consider in the case of a single delay dropping the _1
      ## and going with delay_idx, etc.
      ##
      ## TODO: The Calloc/Free calls here could move into contents if
      ## it took a stage argument.
      delay_i_len <- length(delay_i$extract)

      ## 1. delay_<i>_idx & dim_delay_<i>_idx; indices for deSolve
      delay_i_idx <- sprintf("delay_%d_idx", i)
      type_add(sprintf("dim_%s", delay_i_idx), "int", FALSE)
      type_add(delay_i_idx, "int", TRUE)
      res[[STAGE_CONSTANT]]$add("%s->dim_%s = %d;",
                                name_pars, delay_i_idx, delay_i_len)
      res[[STAGE_CONSTANT]]$add("%s->%s = (int*)Calloc(%s->dim_%s, int);",
                                name_pars, delay_i_idx, name_pars, delay_i_idx)
      free$add("Free(%s->%s);", name_pars, delay_i_idx)

      ## 2. delay_<i>_state & dim_delay_<i>_state; memory for deSolve
      ## (NOTE: this is unfortunate in the dim_ variable)
      delay_i_state <- sprintf("delay_%d_%s", i, STATE)
      type_add(sprintf("dim_%s", delay_i_state), "int", FALSE)
      type_add(delay_i_state, "double", TRUE)
      res[[STAGE_CONSTANT]]$add("%s->dim_%s = %d;",
                                name_pars, delay_i_state, delay_i_len)
      res[[STAGE_CONSTANT]]$add(
                             "%s->%s = (double*)Calloc(%s->dim_%s, double);",
                             name_pars, delay_i_state, name_pars, delay_i_state)
      delay_offset <-
        vcapply(dat$variable_order$offset_use[delay_i$extract], rewrite)
      ## TODO: If the offsets are stored in the structure, point at
      ## those instead?  Look for "offset_is_var"
      res[[STAGE_CONSTANT]]$add(
                             "%s->%s[%d] = %s;",
                             name_pars, delay_i_idx,
                             seq_along(delay_offset) - 1L,
                             delay_offset)
      free$add("Free(%s->%s);", name_pars, delay_i_state)

      ## 3. Prepare output variables so we can push them up out of
      ## scope:
      delay_c$add("double %s;", delay_i$names)
      delay_c$add("{")

      ## TODO: Getting this correct is going to require that we can
      ## output the bits from the big loop+conditional on demand;
      ## that's going to be a challenge to get right, but will require
      ## some functionalisation.  If this approach fails I will
      ## backtrack and and refactor this mess first.

      ## 4. Pull things out of the lag value, but only if time is past
      ## where the lag is OK to work with.  That's going to look like:
      ## TODO: when working with arrays, some of these are *<x>.
      delay_c$add("  double %s;",
                  paste(delay_i$extract, collapse=", "))
      ## TODO: delay times are going to matter a lot here.  This block
      ## actually wants running only when the delay is resolved.  So,
      ## need to interleave this in appropriately.
      delay_c$add("  double delay_%s = %s - %s;",
                  TIME, TIME, rewrite(delay_i$time))
      delay_c$add("  if (delay_%s <= %s->initial_%s) {",
                  TIME, name_pars, TIME)
      delay_c$add("    %s = %s->initial_%s;",
                  delay_i$extract, name_pars, delay_i$extract)
      delay_c$add("  } else {")
      ## TODO: These should be added to the lookup list and done with
      ## rewrite.
      delay_c$add("    lagvalue(delay_%s, %s->%s, %s->dim_%s, %s->%s);",
                  TIME, name_pars, delay_i_idx, name_pars, delay_i_idx,
                  name_pars, delay_i_state)
      ## Then dump out all the values.  TODO: when we allow arrays in
      ## here, then offsets will be required, and the initialisation
      ## looks a little different. For now this is a little easier
      ## though, because everything is a number!
      delay_c$add("    %s = %s->%s[%d];",
                  delay_i$extract, name_pars, delay_i_state,
                  seq_along(delay_i$extract) - 1L)
      delay_c$add("  }")

      ## Then we'll organise that whenever we hit a variable that is
      ## used in a delay statement we'll add it in here in the
      ## appropriate order.

      ## TODO: this will need a little more work for the array case;
      ## by then we'll really need the rhs writing stuff factored out.
      for (nm in delay_i$order) {
        delay_c$add("  double %s = %s;", nm,
                    rewrite(dat$eqs[[match(nm, nms)]]$rhs$value))
      }
      for (nm in delay_i$names) {
        delay_c$add("  %s = %s;", nm,
                    rewrite(dat$eqs[[match(nm, nms)]]$rhs$value_expr))
      }

      delay_c$add("}")
      delay[[i]] <- delay_c
    }
  }

  copy$add("SEXP %s = PROTECT(allocVector(REALSXP, %s));",
           STATE, rewrite(dat$variable_order$total_use))
  order$add("SEXP %s_len = PROTECT(allocVector(INTSXP, %d));",
            STATE, length(dat$variable_order$offset))
  order$add("SEXP %s_names = PROTECT(allocVector(STRSXP, %d));",
            STATE, length(dat$variable_order$offset))
  for (i in seq_along(dat$variable_order$offset)) {
    nm <- dat$variable_order$order[[i]]
    if (dat$variable_order$is_array[[i]]) {
      offset <- rewrite(dat$variable_order$offset_use[[nm]])
      deriv$add("double *%s = %s + %s;", nm, STATE, offset)
      deriv$add("double *deriv_%s = %s + %s;", nm, DSTATEDT, offset)
      copy$add(
        "memcpy(REAL(%s) + %s, %s->initial_%s, %s->dim_%s * sizeof(double));",
        STATE, offset, name_pars, nm, name_pars, nm)
      order$add("INTEGER(%s_len)[%s] = %s->dim_%s;",
                STATE, i - 1L, name_pars, nm)
    } else {
      deriv$add("double %s = %s[%s];", nm, STATE,
                dat$variable_order$offset_use[[i]])
      copy$add("REAL(%s)[%s] = %s->initial_%s;",
               STATE, dat$variable_order$offset_use[[nm]], name_pars, nm)
      order$add("INTEGER(%s_len)[%s] = 1;", STATE, i - 1L)
    }
    order$add("SET_STRING_ELT(%s_names, %d, mkChar(\"%s\"));",
              STATE, i - 1L, nm)
  }
  copy$add("UNPROTECT(1);")
  copy$add("return %s;", STATE)

  order$add("setAttrib(%s_len, R_NamesSymbol, %s_names);", STATE, STATE)
  order$add("UNPROTECT(2);")
  order$add("return %s_len;", STATE)

  ii <- match(dat$order, nms)

  for (i in match(dat$order, nms)) {
    x <- dat$eqs[[i]]
    nm <- x$name

    if (identical(x$lhs$special, "dim")) {
      nm_t <- x$lhs$name_target
      is_var <- nm_t %in% dat$vars
      nm_s <- if (is_var) paste0("initial_", nm_t) else nm_t
      type_add(nm, "int", FALSE)
      type_add(nm_s, "double", TRUE)

      if (x$nd > 1L) {
        ## If allowed here, we'll generate:
        ##   dim_%s_%d % (nm, seq_along(nd))
        ##   dim_%s needs to be the product of these, and done last.
        ##   for nd 3 dim_%s_12 as dim_%s_1 * dim_%s_2, which is used
        ##     in matrix arithmetic
        stop("Multidimensional arrays not yet supported") # TODO
      } else {
        res[[x$stage]]$add("%s->%s = %s;",
                           name_pars, nm, rewrite(x$rhs$value))
      }

      if (is_var) {
        nm_offset <- paste0("offset_", nm_t)
        type_add(nm_offset, "int", FALSE)
        res[[x$stage]]$add("%s->%s = %s;", name_pars, nm_offset,
                           rewrite(dat$variable_order$offset[[nm_t]]))
      }

      ## TODO: I don't see that this is actually possible anymore.
      ## Consider asserting elsewhere and deleting.
      if (x$stage == STAGE_INITIAL) {
        res[[STAGE_CONSTANT]]$add("%s->%s = NULL;", name_pars, nm_s)
        res[[STAGE_INITIAL]]$add("if (%s->%s != NULL) {", name_pars, nm_s)
        res[[STAGE_INITIAL]]$add("  Free(%s->%s);", name_pars, nm_s)
        res[[STAGE_INITIAL]]$add("}")
      }

      res[[x$stage]]$add("%s->%s = (double*) Calloc(%s->%s, double);",
                         name_pars, nm_s, name_pars, nm)
      free$add("Free(%s->%s);", name_pars, nm_s)
    } else if (isTRUE(x$rhs$delay)) {
      j <- x$rhs$group_delay
      res[[STAGE_TIME]]$add(delay[[j]]$get())
      delay[[j]]$clear()
    } else if (x$lhs$type == "symbol") {
      if (isTRUE(x$rhs$user)) {
        if (isTRUE(x$rhs$default)) {
          default <- rewrite(x$rhs$value)
        } else {
          default <- if (type == "int") "NA_INTEGER" else "NA_REAL"
        }
        value <- sprintf("%s_get_user_%s(user, \"%s\", %s)",
                         base, type, nm, default)
      } else {
        value <- rewrite(x$rhs$value)
      }
      type <- if (nm %in% dat$index_vars) "int" else "double"
      if (x$stage < STAGE_TIME) {
        type_add(nm, type, FALSE)
        res[[x$stage]]$add("%s->%s = %s;", name_pars, nm, value)
      } else if (identical(x$lhs$special, "deriv")) {
        res[[x$stage]]$add("%s[%s] = %s;",
                           DSTATEDT,
                           dat$variable_order$offset_use[[x$lhs$name_target]],
                           value)
      } else {
        res[[x$stage]]$add("%s %s = %s;", type, nm, value)
      }
    } else if (x$lhs$type == "array") {
      if (isTRUE(x$rhs$user)) {
        ## This will require a bit of woprk here.  We'll support
        ## things like
        ##
        ##   x[a:b] <- user() # compute (b - a + 1) at runtime and check
        ##   x[] <- user()    # dim_x is required length
        ##   x[,1] <- user()  # dim_x_1 is required length
        ##
        ## and so on.  But this is harder to get right so leaving it
        ## for now.
        stop("User-supplied arrays not yet supported") # TODO
      }
      indent <- ""
      for (j in seq_along(x$lhs$index)) {
        xj <- x$lhs$index[[j]]
        is_range <- xj$is_range
        target <- xj$extent_max
        ## TODO: The index variables need sanitising so that no more
        ## than one of i,j,k is allowed; things like x[i,j] = z[i + j]
        ## are not allowed!
        for (k in seq_along(is_range)) {
          if (is_range[k]) {
            res[[x$stage]]$add("%sfor (int %s = %s; %s < %s; ++%s) {",
                               indent,
                               INDEX[[k]], minus1(xj$extent_min[[k]], rewrite),
                               INDEX[[k]], rewrite(xj$extent_max[[k]]),
                               INDEX[[k]])
            indent <- paste0("  ", indent)
            target[[k]] <- as.symbol(INDEX[[k]])
          } else if (INDEX[[k]] %in% x$rhs$depends$variables) {
            ## TODO: I need to get the index rhs depends back here to
            ## do this best (i.e., if the rhs does not depend on an
            ## index then don't bother adding the declaration here).
            ## As it is this will do this for *all* entries which is
            ## not ideal.
            if (!nzchar(indent)) {
              res[[x$stage]]$add("{")
              indent <- "  "
            }
            res[[x$stage]]$add("%sint %s = %s;", indent, INDEX[[k]],
                               minus1(xj$extent_max[[k]], rewrite))
            target[[k]] <- as.symbol(INDEX[[k]])
          } else {
            target[[k]] <- xj$extent_max[[k]]
          }
        }
        target <- rewrite(as.call(c(quote(`[`), as.symbol(nm), target)))
        value <- rewrite(x$rhs$value[[j]])
        res[[x$stage]]$add("%s%s = %s;", indent, target, value)
        while (nzchar(indent)) {
          indent <- substr(indent, 3L, nchar(indent))
          res[[x$stage]]$add("%s}", indent)
        }
      }
    } else {
      stop("Unhandled type")
    }
  }

  ## Add the total information:
  if (dat$variable_order$total_is_var) {
    type_add(dat$variable_order$total_use, "int", FALSE)
    res[[dat$variable_order$total_stage]]$add("%s->%s = %s;",
                                              name_pars,
                                              dat$variable_order$total_use,
                                              rewrite(dat$variable_order$total))
  }

  ## This requires knowing how many types we have, so must be done last.
  contents$prepend("SEXP %s_names = PROTECT(allocVector(STRSXP, %d));",
                   STATE, contents$i)
  contents$prepend("SEXP %s = PROTECT(allocVector(VECSXP, %d));",
                   STATE, contents$i)
  contents$add("setAttrib(%s, R_NamesSymbol, %s_names);", STATE, STATE)
  contents$add("UNPROTECT(2);")
  contents$add("return %s;", STATE)

  f <- function(x) {
    paste(indent(x$get()), collapse="\n")
  }

  data <- list()
  data$struct <- f(types)
  data$free <- f(free)
  data$create <- f(init)
  data$initialise <- f(user)
  data$copy <- f(copy)
  data$deriv <- f(deriv)
  data$contents <- f(contents)
  data$order <- f(order)
  data$time <- TIME
  data$state <- STATE
  data$dstatedt <- DSTATEDT
  data$pars_type <- type_pars
  data$pars_object <- name_pars
  data$prefix <- base
  data
}
