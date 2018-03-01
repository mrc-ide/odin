#' Plotting odin models
#'
#' The \code{plot} method for \code{odin_model} objects is a wrapper around the
#' \code{$code} slot of these objects.
#' @export
#' @importFrom graphics plot
plot.odin_model <- function(x, ...) {
  plot_odin_model(x, ...)
}

## This internal function effectively does the plotting.
plot_odin_model <- function(x, box_size = 10, box_col = NULL,
                             straight = FALSE, ...) {

  out <- NULL
  loadNamespace("visNetwork")

  boxes <- x$names[-1]
  n <- length(boxes)
  if (is.null(box_col)) {
    box_col <- odin_pal(n)
  }
  nodes <- data.frame(id = boxes,
                      label = boxes,
                      shape = "box",
                      color = box_col,
                      size = box_size)

  ## TODO: this will need changing!

  ## we are assuming that each compartment flows only to the next; this is
  ## a placeholder waiting to get the real flows from the structure.

  ## Note: we are not using %>% on purpose to avoid the dependency on
  ## maggritr

  edges <- data.frame(from = boxes[-n],
                      to = boxes[-1],
                      arrows = "to",
                      smooth = !straight)

  out <- visNetwork::visNetwork(nodes = nodes,
                                edges = edges,
                                height = "800px",
                                width = "100%",
                                highlightNearest = TRUE)

  out <- visNetwork::visOptions(out, highlightNearest = TRUE)
  out <- visNetwork::visPhysics(out, stabilization = FALSE)

  out
}


odin_pal <- function(n) {
  assert_scalar_integer(n)
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
