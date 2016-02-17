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
  res <- list(constant=init, user=user, time=deriv)[STAGES]

  body <- list(init, user, deriv)

  name_pars <- sprintf("%s_p", base)
  type_pars <- sprintf("%s_pars", base)

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
           use.names=FALSE))
  ## TODO: Need to add information for all arrays that have more than
  ## one dimension because we'll need to copy things like dim_<x>_1
  ## and dim_<x>_12 over too.

  rewrite <- function(x) {
    rewrite_c(x, name_pars, lookup, INDEX)
  }

  copy$add("SEXP %s = PROTECT(allocVector(REALSXP, %s->%s));",
           STATE, name_pars, dat$variable_order$total_use)
  for (i in seq_along(dat$variable_order$offset)) {
    nm <- dat$variable_order$order[[i]]
    if (dat$variable_order$is_array[[i]]) {
      offset <- rewrite(dat$variable_order$offset_use[[nm]])
      deriv$add("double *%s = %s + %s;", nm, STATE, offset)
      deriv$add("double *deriv_%s = %s + %s;", nm, DSTATEDT, offset)
      copy$add(
        "memcpy(REAL(%s) + %s, %s->initial_%s, %s->dim_%s * sizeof(double));",
        STATE, offset, name_pars, nm, name_pars, nm)
    } else {
      deriv$add("double %s = %s[%s];", nm, STATE,
                dat$variable_order$offset_use[[i]])
      copy$add("REAL(%s)[%s] = %s->initial_%s;",
               STATE, dat$variable_order$offset_use[[x$lhs$name_target]],
               name_pars, nm)
    }
  }
  copy$add("UNPROTECT(1);")
  copy$add("return %s;", STATE)

  ## TODO: Still have to write the user-processing bits yet.  That
  ## should probably be: "try to read an element from the list, and if
  ## that fails then set to the default value".
  nms <- vcapply(dat$eqs, function(x) x$lhs$name)
  ii <- match(dat$order, nms)

  for (i in match(dat$order, nms)) {
    x <- dat$eqs[[i]]
    nm <- x$name

    if (identical(x$lhs$special, "dim")) {
      nm_t <- x$lhs$name_target
      is_var <- nm_t %in% dat$vars
      nm_s <- if (is_var) paste0("initial_", nm_t) else nm_t
      types$add("int %s;", nm)
      types$add("double *%s;", nm_s)

      if (x$nd > 1L) {
        ## If allowed here, we'll generate:
        ##   dim_%s_%d % (nm, seq_along(nd))
        ##   dim_%s needs to be the product of these, and done last.
        ##   for nd 3 dim_%s_12 as dim_%s_1 * dim_%s_2, which is used
        ##     in matrix arithmetic
        stop("Multidimensional arrays not yet supported")
      } else {
        res[[x$stage]]$add("%s->%s = %s;",
                           name_pars, nm, rewrite(x$rhs$value))
      }

      if (is_var) {
        nm_offset <- paste0("offset_", nm_t)
        types$add("int %s;", nm_offset)
        res[[x$stage]]$add("%s->%s = %s;", name_pars, nm_offset,
                           rewrite(dat$variable_order$offset[[nm_t]]))
      }

      if (x$stage == STAGE_USER) {
        res[[STAGE_CONSTANT]]$add("%s->%s = NULL;", name_pars, nm_s)
        res[[STAGE_USER]]$add("if (%s->%s != NULL) {", name_pars, nm_s)
        res[[STAGE_USER]]$add("  Free(%s->%s);", name_pars, nm_s)
        res[[STAGE_USER]]$add("}")
      }

      res[[x$stage]]$add("%s->%s = (double*) Calloc(%s->%s, double);",
                         name_pars, nm_s, name_pars, nm)
      free$add("Free(%s->%s);", name_pars, nm_s)
    } else if (x$lhs$type == "symbol") {
      type <- if (nm %in% dat$index_vars) "int" else "double"
      if (x$stage < STAGE_TIME) {
        types$add("%s %s;", type, nm)
        res[[x$stage]]$add("%s->%s = %s;", name_pars, nm,
                           rewrite(x$rhs$value))
      } else if (identical(x$lhs$special, "deriv")) {
        res[[x$stage]]$add("%s[%s] = %s;",
                           DSTATEDT,
                           dat$variable_order$offset_use[[x$lhs$name_target]],
                           rewrite(x$rhs$value))
      } else {
        res[[x$stage]]$add("%s %s = %s;", type, nm,
                           rewrite(x$rhs$value))
      }
    } else if (x$lhs$type == "array") {
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
    types$add("int %s;", dat$variable_order$total_use)
    res[[dat$variable_order$total_stage]]$add("%s->%s = %s;",
                                              name_pars,
                                              dat$variable_order$total_use,
                                              rewrite(dat$variable_order$total))
  }

  data <- list()
  data$struct <- paste(indent(types$get()), collapse="\n")
  data$free <- paste(indent(free$get()), collapse="\n")
  data$create <- paste(indent(init$get()), collapse="\n")
  data$initialise <- paste(indent(user$get()), collapse="\n")
  data$copy <- paste(indent(copy$get()), collapse="\n")
  data$deriv <- paste(indent(deriv$get()), collapse="\n")
  data$time <- TIME
  data$state <- STATE
  data$dstatedt <- DSTATEDT
  data$pars_type <- type_pars
  data$pars_object <- name_pars
  data$prefix <- base
  data
}
