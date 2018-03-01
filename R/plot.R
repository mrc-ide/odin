##' Plotting odin models
##'
##' The \code{plot} method for \code{odin_model} objects is a wrapper around the
##' \code{$code} slot of these objects.
##' @export
##' @importFrom graphics plot
plot.odin_model <- function(x, ...) {
  build_odin_graph(x$graph_data(), ...)
}


##' @export
##' @rdname plot.odin_model
plot.odin_generator <- function(x, ...) {
  build_odin_graph(attr(x, "graph_data")(), ...)
}


build_odin_graph <- function(graph, box_size = 10, box_col = NULL,
                            straight = FALSE, height = "800px", width = "100%",
                            ...) {
  loadNamespace("visNetwork")

  nodes <- graph$nodes
  nodes$shape <- "box"
  nodes$size <- box_size
  nodes$color <- box_col %||% odin_pal(nrow(nodes))

  edges <- graph$edges
  edges$arrows <- "to"
  edges$smooth <- !straight

  out <- visNetwork::visNetwork(nodes = nodes, edges = edges,
                                height = height, width = width,
                                highlightNearest = TRUE)
  out <- visNetwork::visOptions(out, highlightNearest = TRUE)
  out <- visNetwork::visPhysics(out, stabilization = FALSE)

  out
}


odin_pal <- function(n) {
  n <- as.integer(n)
  colors <- c("#ccddff", "#79d2a6", "#ffb3b3",
              "#a4a4c1", "#ffcc00", "#ff9f80",
              "#ccff99", "#df9fbf", "#ffcc99",
              "#cdcdcd")
  if (n < length(colors)) {
    colors[seq_len(n)]
  } else {
    grDevices::colorRampPalette(colors)(n)
  }
}
