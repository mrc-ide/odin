
#' Color tools and palettes for epicontacts
#'
#' These functions are used for defining palettes or colors in the
#' \code{epicontacts} package. They include:
#'
#' \itemize{
#'
#' \item \code{odin_pal}: discrete color palette used for cases (comes from the
#' \code{dibbler} package)
#'
#' \item \code{spectral}: continuous color palette (comes from the
#' \code{adegenet} package)
#'
#' \item \code{transp}: makes colors transparent (comes from the
#' \code{adegenet} package)
#'
#' \item \code{char2col}: translates a character or a factor to a color using a
#' palette (comes from the \code{adegenet} package)
#' 
#' }
#'
#'
#' @export
#'
#' @rdname colors
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @param n An integer indicating the number of colors.
#'
#' @examples
#'
#' barplot(1:5, col = odin_pal(5))
#' barplot(1:50, col = odin_pal(50))
#' 

odin_pal <- function(n){
  if (!is.numeric(n)) {
    stop("n is not a number")
  }
  n <- as.integer(n)
  colors <- c("#ccddff", "#79d2a6", "#ffb3b3",
              "#a4a4c1", "#ffcc00", "#ff9f80",
              "#ccff99", "#df9fbf", "#ffcc99",
              "#cdcdcd")
  if (n < length(colors)) {
    return(colors[seq_len(n)])
  } else {
    return(grDevices::colorRampPalette(colors)(n))
  }
}

