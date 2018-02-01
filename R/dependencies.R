find_symbols <- function(expr, hide_errors = TRUE) {
  if (is.list(expr)) {
    return(join_deps(lapply(expr, find_symbols)))
  }
  functions <- variables <- character(0)

  f <- function(e) {
    if (!is.recursive(e)) {
      if (!is.symbol(e)) { # A literal of some type
        return()
      }
      variables <<- c(variables, deparse(e))
    } else {
      nm <- deparse(e[[1L]])
      if (nm %in% c("dim", "length")) {
        ## These functions are treated separately because length(X) does
        ## not depend on the value of X so much as the *length*.  That's
        ## handled by a separate variable that we hook up here.
        if (length(e) >= 2L) {
          ## The if here avoids an invalid parse, e.g. length(); we'll
          ## pick that up later on.
          variables <<- c(variables, array_dim_name(deparse(e[[2L]])))
        }
        ## Still need to declare the function as used because we'll
        ## want to check that later.
        functions <<- c(functions, nm)
      } else {
        functions <<- c(functions, deparse(e[[1]]))
        for (a in as.list(e[-1])) {
          if (!missing(a)) {
            f(a)
          }
        }
      }
    }
  }

  f(expr)
  list(functions = unique(functions),
       variables = unique(variables))
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
  no_dep <- lengths(graph) == 0L
  graph_sorted <- names(no_dep[no_dep])
  graph <- graph[!no_dep]

  while (length(graph) > 0L) {
    acyclic <- FALSE
    for (i in seq_along(graph)) {
      edges <- graph[[i]]
      if (!any(edges %in% names(graph))) {
        acyclic <- TRUE
        graph_sorted <- c(graph_sorted, names(graph[i]))
        graph <- graph[-i]
        break
      }
    }
    if (!acyclic) {
      f <- function(x) {
        ## Note that this is not going to give the right answer here
        ## but it might still be useful (dim_x -> dim(x), initial_x ->
        ## initial(x) etc.)  Could swap these around with
        ## RESERVED_PREFIX perhaps.
        y <- graph[[x]]
        i <- vlapply(graph[y], function(el) x %in% el)
        sprintf("\t%s: depends on %s", x, y[i])
      }
      err <- intersect(edges, names(graph))
      stop(sprintf("A cyclic dependency detected for %s:\n%s",
                   paste(err, collapse = ", "),
                   paste(vcapply(err, f), collapse = "\n")), call. = FALSE)
    }
  }

  graph_sorted
}
