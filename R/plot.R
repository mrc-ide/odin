#' Plotting odin models
#'
#' The \code{plot} method for \code{odin_model} objects is a wrapper around the
#' \code{$code} slot of these objects.

## Note on defining S3 methods for R6 objects:
##
## The 'proper' way is to define a slot '$f' in the object where 'f' is a S3
## generic, which effectively contains the code for the method, and then
## register the method using `f.foobar <- function(x) x$f` where `x` is an R6
## object of class `foobar`.
##
## For the time being the plotting won't touch the class, but once it's done the
## following code should be used (maybe adding frequent args to the signature):
## 
## plot.odin_model <- function(x, ...) {
##     x$plot(...)
## }


#' @export
#' @importFrom graphics plot

plot.odin_model <- function(x, ...) {
    .plot_odin_model(x, ...)
}


## This internal function effectively does the plotting.
.plot_odin_model <- function(x, box_size = 10, box_col = NULL,
                             straight = FALSE, ...) {

    out <- NULL
    if (requireNamespace("visNetwork")) {
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
    }

    return(out)
}
