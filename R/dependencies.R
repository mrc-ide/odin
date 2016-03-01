find_symbols <- function(expr, hide_errors=TRUE) {
  if (is.list(expr)) {
    return(join_deps(lapply(expr, find_symbols)))
  }
  functions <- variables <- character(0)

  leaf <- function(e, w) {
    if (!is.symbol(e)) { # A literal of some type
      return()
    }
    variables <<- c(variables, deparse(e))
  }
  call <- function (e, w) {
    nm <- deparse(e[[1L]])
    if (nm %in% c("dim", "length")) {
      ## These functions are treated separately because length(X) does
      ## not depend on the value of X so much as the *length*.  That's
      ## handled by a separate variable that we hook up here.
      if (length(e) >= 2L) {
        ## The if here avoids an invalid parse, e.g. length(); we'll
        ## pick that up later on.
        variables <<- c(variables, paste0("dim_", deparse(e[[2L]])))
      }
    } else {
      functions <<- c(functions, deparse(e[[1]]))
      for (a in as.list(e[-1])) {
        if (!missing(a)) {
          codetools::walkCode(a, w)
        }
      }
    }
  }

  walker <- codetools::makeCodeWalker(call=call, leaf=leaf, write=cat)
  codetools::walkCode(expr, walker)
  list(functions=unique(functions),
       variables=unique(variables))
}

join_deps <- function(x) {
  stopifnot(is.list(x))
  x <- x[!vlapply(x, is.null)]
  ## This should never be triggered
  ok <- vlapply(x, function(el)
    identical(names(el), c("functions", "variables")))
  stopifnot(all(ok))
  if (length(x) == 0L) {
    list(functions=character(0), variables=character(0))
  } else if (length(x) == 1L) {
    x[[1L]]
  } else {
    list(functions=unique(unlist(lapply(x, "[[", "functions"))),
         variables=unique(unlist(lapply(x, "[[", "variables"))))
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
      stop("A cyclic dependency detected for: ",
           paste(intersect(edges, names(graph)), collapse=", "))
    }
  }

  graph_sorted
}
