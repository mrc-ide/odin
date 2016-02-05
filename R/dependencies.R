find_symbols <- function(expr, hide_errors=TRUE) {
  functions <- variables <- character(0)

  leaf <- function(e, w) {
    if (!is.symbol(e)) { # A literal of some type
      return()
    }
    variables <<- c(variables, deparse(e))
  }
  call <- function (e, w) {
    functions <<- c(functions, deparse(e[[1]]))
    for (a in as.list(e[-1])) {
      if (!missing(a)) {
        codetools::walkCode(a, w)
      }
    }
  }

  walker <- codetools::makeCodeWalker(call=call, leaf=leaf, write=cat)
  codetools::walkCode(expr, walker)
  list(functions=unique(functions),
       variables=unique(variables),
       variables_count=table(variables))
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
      stop("A cyclic dependency detected")
    }
  }

  graph_sorted
}
