## These are all "odin constants".  Some of these we'll probably make
## user configurable at some point (time is one, the index variables
## are another).  Most of the others don't really need to change
## unless it becomes really limiting to hit name collisions (so
## rewriting names to get them out the way).
STAGE_CONSTANT <- 1L
STAGE_USER <- 2L
STAGE_TIME <- 3L
STAGE_OUTPUT <- 4L
STAGES <- c("constant", "user", "time", "output")
TIME <- "t"
STATE <- "state"
DSTATEDT <- "dstatedt"
OUTPUT <- "output"
USER <- "user"
## TODO: None of these deal with the use of these as functions (only
## variables) but that needs checking too.  Not 100% sure this is done
## on the lhs index bits.  Probably need to standardise that at some
## point.
SPECIAL_LHS <- c("initial", "deriv", "output", "dim", "config")
SPECIAL_RHS <- c("user", "interpolate", "delay")
INDEX <- c("i", "j", "k")
RESERVED <- c(INDEX, TIME, STATE, DSTATEDT, USER, SPECIAL_LHS, "delay", "dde")
RESERVED_PREFIX <- c(SPECIAL_LHS, "odin", "offset", "delay", "interpolate")
VALID_ARRAY <- c("-", "+", ":", "(", "length", "dim")
INTERPOLATION_TYPES <- c("constant", "linear", "spline")

## Avoid a lot of error print pasting:
array_dim_name <- function(name, sub=NULL, use=TRUE) {
  if (length(name) > 1L) {
    return(vcapply(name, array_dim_name, sub, use, USE.NAMES=FALSE))
  }
  if (!is.null(sub)) {
    name <- sprintf("%s_%s", name, sub)
  }
  if (grepl("^(initial|deriv)_", name)) {
    name_dim <- sub("^(initial|deriv)_", "dim_", name)
  } else if (grepl("^delay_", name)) {
    re <- "^delay_([^_]+)_(.*)$"
    type <- sub(re, "\\1", name)
    if (type == INDEX[[1L]] || (use && type == STATE)) {
      name_dim <- sub(re, "dim_delay_\\2", name)
    } else if (use) {
      name_dim <- sub("^delay_", "dim_", name)
    } else {
      name_dim <- NULL
    }
  } else {
    name_dim <- sprintf("dim_%s", name)
  }
  name_dim
}

delay_name <- function(name) {
  sprintf("delay_%s", name)
}

initial_name <- function(name) {
  sprintf("initial_%s", name)
}
