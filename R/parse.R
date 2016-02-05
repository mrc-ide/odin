## Read in the file and do the basic classification of all expressions.
odin_parse <- function(file="", text=NULL) {
  expr <- parse(file=file, text=text, keep.source=TRUE)

  ## First pass is to check that all operations are assignments.  No
  ## for loops, no if/else statements.  This might be relaxed later to
  ## allow if/else blocks to be created, but that's going to be
  ## potentially difficult because we'll need to check a bunch of
  ## branches.
  is_assignment <- function(x) {
    length(x) == 3L &&
      (identical(x[[1]], quote(`<-`)) || identical(x[[1]], quote(`=`)))
  }
  err <- which(!vlapply(expr, is_assignment))
  if (length(err) > 0L) {
    stop(paste(c("Every line must contain an assignment",
                 paste0("\t", vcapply(err, pretty_ref, expr))), collapse="\n"))
  }
  lhs <- lapply(expr, function(x) odin_parse_lhs(x[[2L]]))
  rhs <- lapply(expr, function(x) odin_parse_rhs(x[[3L]]))
  names(lhs) <- names(rhs) <- vcapply(lhs, "[[", "name")

  ## First, pull out the derivatives; these *must* be dealt with
  ## separately as they will have at least two entries; one for the
  ## initial conditions and one for the derivatives themselves.  Array
  ## initial conditions will need to be done in here too, but are not
  ## handled yet either (see lorenz.R)
  ##
  ## TODO: array are assumed to be able to processed all at once with
  ## the total of all their dependencies.  That means you can't have
  ## two matrices that have complex interations with one another
  ## though it means you can do one matrix lagging another.
  i <- vlapply(lhs, function(x) identical(x$special, "deriv"))
  deriv <- list(lhs=lhs[i], rhs=rhs[i])

  j <- vlapply(lhs, function(x) identical(x$special, "initial"))
  initial <- list(lhs=lhs[j], rhs=rhs[j])

  k <- i | j
  vars <- list(lhs=lhs[!k], rhs=rhs[!k])

  list(deriv=combine_arrays(deriv),
       initial=combine_arrays(initial),
       vars=combine_arrays(vars))
}

## Ideally I think we need to handle functions of the form:
##   symbol <- ...
##   array[i, j, k] <- ...
##   array[] <- ...
##   initial(symbol) <- ...
##   deriv(symbol) <- ...
## any others?
odin_parse_lhs <- function(lhs) {
  ## TODO: get the source information to follow along here; should at
  ## least indicate the line number for each symbol, I think.
  if (is.name(lhs)) {
    list(type="symbol",
         name=deparse(lhs))
  } else if (is.call(lhs)) {
    fun <- deparse(lhs[[1]])

    ## TODO: For the special functions, store the variables as
    ## initial(foo) and deriv(foo) to distinguish from the foo that
    ## floats around in the system implicitly.
    special <- c("initial", "deriv")
    ## Consider allowing "[["?
    if (fun %in% "[") {
      stopifnot(is.name(lhs[[2L]]))
      list(type="array",
           name=deparse(lhs[[2L]]),
           index=as.list(lhs[-(1:2)]))
    } else if (fun %in% special) {
      if (length(lhs) != 2L) {
        stop("Invalid length special function: ", deparse_str(lhs))
      }
      ret <- odin_parse_lhs(lhs[[2L]])
      if (is.null(ret$special)) {
        ret$special <- fun
      } else {
        ## This should be called on the *final* iteration; this will
        ## give the wrong error message on
        ##   initial(initial(initial(x))) <- ...
        ## but noone should do that!
        stop("Invalid length special function: ", deparse_str(lhs))
      }
      ret
    } else {
      stop("Unhandled expression ", fun)
    }
  } else {
    stop("I'm confused")
  }
}

## First, go through and see what they depend on.
odin_parse_rhs <- function(rhs) {
  if (is.atomic(rhs)) {
    ## These are easy; they're constants so we can deal with these directly.
    ##
    ## Should check there that everything is of the classes: integer,
    ## logical, numeric only.  It's possible that strings would be
    ## possible but I'm not sure that's sensible.
    list(type="atomic", value=rhs)
  } else {
    stopifnot(is.call(rhs) || is.name(rhs))
    list(type="expression",
         depends=find_symbols(rhs),
         value=rhs)
  }
}

combine_arrays <- function(dat) {
  lhs <- dat$lhs
  rhs <- dat$rhs
  nms <- names(lhs)

  dups <- unique(nms[duplicated(nms)])
  if (any(dups)) {
    keep <- rep_len(TRUE, length(nms))
    for (i in dups) {
      j <- which(nms == i)
      k <- j[[1L]]
      nm <- nms[[k]]
      type_i <- vcapply(lhs[j], "[[", "type")
      if (any(type_i != "array")) {
        stop(sprintf("Duplicate arguments must all be arrays (%s: %s)",
                     nm, paste(type_i, collapse=", ")))
      }
      lhs[[k]]$index <- lapply(lhs[j], "[[", "index")
      ## Combine the rhs;
      f <- function(x, i) {
        unique(as.character(unlist(lapply(x, function(x) x$depends[[i]]))))
      }
      tmp <- list(type="array",
                  depends=list(functions=f(rhs[j], "functions"),
                               variables=f(rhs[j], "variables")),
                  value=lapply(rhs[j], "[[", "value"))
      ## TODO: check the form of the self referential variables; there
      ## is a limited set of allowed cases really but I don't think
      ## it's going to be easy to validate them all.
      tmp$depends$variables <- setdiff(tmp$depends$variables, nm)
      rhs[[k]] <- tmp
      keep[j[-1L]] <- FALSE
    }
    lhs <- lhs[keep]
    rhs <- rhs[keep]
  }

  list(lhs=lhs, rhs=rhs)
}
