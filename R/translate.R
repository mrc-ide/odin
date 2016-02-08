num_to_str <- function(x, control="hexNumeric") {
  if (!is.numeric(x) || length(x) != 1L) {
    stop("invalid input")
  }
  str <- as.character(x)
  if (identical(as.numeric(str), x)) {
    if (!grepl(".", str, fixed=TRUE)) {
      str <- paste0(str, ".0")
    }
  } else {
    str <- deparse(x, control=control)
  }
  str
}

## The next question is how much do we deal with list accessing
## initial conditions?
deparse_str_c <- function(x) {
  ## Do C translation here:
  ##   ^ -> pow
  ## or do I do that in the parse step?
  deparse_str(x)
}

## For the R->C translation we want to build up a string bit by bit.
## But R's strings are immutable and this is likely to be slow.  But
## then this is a compile step so it's not that big a deal.
##
## Done right this could be quite useful generally.
