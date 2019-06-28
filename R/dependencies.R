find_symbols <- function(expr, hide_errors = TRUE) {
  if (is.list(expr)) {
    return(join_deps(lapply(expr, find_symbols)))
  }
  functions <- collector()
  variables <- collector()

  f <- function(e) {
    if (!is.recursive(e)) {
      if (!is.symbol(e)) { # A literal of some type
        return()
      }
      variables$add(deparse(e))
    } else {
      nm <- deparse(e[[1L]])
      if (nm %in% c("dim", "length")) {
        ## These functions are treated separately because length(X) does
        ## not depend on the value of X so much as the *length*.  That's
        ## handled by a separate variable that we hook up here.
        if (length(e) >= 2L) {
          ## The if here avoids an invalid parse, e.g. length(); we'll
          ## pick that up later on.
          variables$add(array_dim_name(deparse(e[[2L]])))
        }
        ## Still need to declare the function as used because we'll
        ## want to check that later.
        functions$add(nm)
      } else {
        functions$add(deparse(e[[1]]))
        for (a in as.list(e[-1])) {
          if (!missing(a)) {
            f(a)
          }
        }
      }
    }
  }

  f(expr)
  list(functions = unique(functions$get()),
       variables = unique(variables$get()))
}

join_deps <- function(x) {
  stopifnot(is.list(x))
  x <- x[!vlapply(x, is.null)]
  ## This should never be triggered
  ok <- vlapply(x, function(el)
    identical(names(el), c("functions", "variables")))
  stopifnot(all(ok))
  if (length(x) == 0L) {
    list(functions = character(0), variables = character(0))
  } else if (length(x) == 1L) {
    x[[1L]]
  } else {
    list(functions = unique(unlist(lapply(x, "[[", "functions"))),
         variables = unique(unlist(lapply(x, "[[", "variables"))))
  }
}

## This algorithm comes from here:
## http://blog.jupo.org/2012/04/06/topological-sorting-acyclic-directed-graphs/
## and assumes that the graph is expressed as a *named* list.  The
## daughters of an element are its dependencies.
topological_order <- function(graph) {
  m <- matrix(FALSE, length(graph), length(graph))
  for (i in seq_along(graph)) {
    m[, i] <- unname(names(graph) %in% graph[[i]])
  }

  pending <- rep(TRUE, length(graph))
  graph_sorted <- integer(0)
  while (any(pending)) {
    i <- which(pending)[colSums(m[, pending, drop = FALSE]) == 0]
    if (length(i) > 0L) {
      graph_sorted <- c(graph_sorted, i)
      pending[i] <- FALSE
      m[i, ] <- FALSE
    } else {
      f <- function(i) {
        ## Note that this is not going to give the right answer here
        ## but it might still be useful (dim_x -> dim(x), initial_x ->
        ## initial(x) etc.)  Could swap these around with
        ## RESERVED_PREFIX perhaps.
        sprintf("\t%s: depends on %s",
                names(graph)[[i]], paste(err[m[pending, i]], collapse = ", "))
      }
      err <- names(graph)[pending]
      detail <- paste(vcapply(which(pending), f), collapse = "\n")
      stop(sprintf("A cyclic dependency detected for %s:\n%s",
                   paste(names(graph)[pending], collapse = ", "),
                   detail), call. = FALSE)
    }
  }

  names(graph)[graph_sorted]
}


recursive_dependencies <- function(order, deps, vars) {
  deps_rec <- setNames(vector("list", length(order)), order)
  for (i in order) {
    j <- as.character(unlist(deps[i]))
    deps_rec[[i]] <-
      c(j, unique(as.character(unlist(deps_rec[j], use.names = FALSE))))
  }
  deps_rec
}
