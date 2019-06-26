## These are all "odin constants".  Some of these we'll probably make
## user configurable at some point (time is one, the index variables
## are another).  Most of the others don't really need to change
## unless it becomes really limiting to hit name collisions (so
## rewriting names to get them out the way).
STAGE_NULL <- 0L
STAGE_CONSTANT <- 1L
STAGE_USER <- 2L
STAGE_TIME <- 3L

TIME <- "t"
STEP <- "step"
STATE <- "state"
DSTATEDT <- "dstatedt"
STATE_NEXT <- "state_next"
OUTPUT <- "output"
USER <- "user"
RING <- "odin_ring"
## TODO: None of these deal with the use of these as functions (only
## variables) but that needs checking too.  Not 100% sure this is done
## on the lhs index bits.  Probably need to standardise that at some
## point.
SPECIAL_LHS <- c("initial", "deriv", "update", "output", "dim", "config")
SPECIAL_RHS <- c("user", "interpolate", "delay")
INDEX <- c("i", "j", "k", "l", "i5", "i6", "i7", "i8") # TODO: make open
INTERNAL <- "internal"
RESERVED <- c(INDEX, TIME, STEP, STATE, DSTATEDT, STATE_NEXT, USER,
              SPECIAL_LHS, "delay", "dde", INTERNAL)
RESERVED_PREFIX <- c(SPECIAL_LHS, "odin", "offset", "delay", "interpolate")
VALID_ARRAY <- c("-", "+", ":", "(", "length", "dim", "[")
INTERPOLATION_TYPES <- c("constant", "linear", "spline")
SPECIAL_DATA_TYPES <- c("void", "ring_buffer")

DIM_USER <- -1L
DIM_DEPENDENT <- -2L

DLL_PLACEHOLDER <- "<<ODIN_DLL>>"

## These are going to be in a list of 0, 1, 2, ... arguments.  The
## last category will be nary.

FUNCTIONS_INFIX <- c("+", "/", "-", "*", ">", "<", ">=", "<=", "==", "!=",
                     "&&", "||")
FUNCTIONS_UNARY <- c("+", "-") # TODO: add "!"?

FUNCTIONS_RENAME <- c(
  "%%" = "fmodr",
  "%/%" = "fintdiv",
  "^" = "pow",
  abs = "fabs",
  max = "fmax",
  min = "fmin",
  gamma = "gammafn",
  lgamma = "lgammafn",
  ceiling = "ceil"
)

## TODO: if INDEX is open-ended, this becomes open-ended too
FUNCTIONS_SUM <- sprintf("odin_sum%d", seq_along(INDEX))

FUNCTIONS <- list(
  ## Things that get special treatment
  "[" = NA,   # checked in the array code
  interpolate = NA, # dealt with elsewhere
  sum = 1L,

  ## General
  "(" = 1L,
  length = 1L,
  dim = 2L,
  "if" = 3L,
  ## Mathematical operations
  pow = 2L,
  fabs = 1L,
  fmodr = 2L,
  fmin = c(2L, Inf),
  fmax = c(2L, Inf),
  exp = 1L,
  log = c(1L, 2L),
  log2 = 1L,
  log10 = 1L,
  log1p = 1L,
  expm1 = 1L,
  sqrt = 1L,
  lgammafn = 1L,
  gammafn = 1L,
  beta = 2L,
  lbeta = 2L,
  choose = 2L,
  lchoose = 2L,
  sign = 1L,
  ## Rounding
  round = 1L,
  trunc = 1L,
  floor = 1L,
  ceil = 1L,
  ## Big pile of trig:
  cos = 1L,   sin = 1L,   tan = 1L,
  acos = 1L,  asin = 1L,  atan = 1L,  atan2 = 2L,
  cosh = 1L,  sinh = 1L,  tanh = 1L,
  acosh = 1L, asinh = 1L, atanh = 1L
)

FUNCTIONS_STOCHASTIC <- list(
  ## Support the standard distribution functions (faster than below)
  unif_rand = 0L,
  norm_rand = 0L,
  exp_rand = 0L,
  ## And support many different distributions
  rbeta = 2L, # a, b
  rbinom = 2L, # n, p
  rcauchy = 2L, # location, scale
  rchisq = 1L, # df
  rexp = 1L, # scale (and not rate) TODO: rewrite
  rf = 2L, # n1, n2
  rgamma = 2L, # shape, scale
  rgeom = 1L, # p
  rhyper = 3L, # NR, NB, n
  rlogis = 2L, # location, scale
  rlnorm = 2L, #	logmean, logsd
  rnbinom = 2L, # size, prob
  rnorm = 2L, # mu, sigma
  rpois = 1L, # lambda
  rt = 1L, # n
  runif = 2L, # a, b
  rweibull = 2L, # shape, scale
  rwilcox = 2L, # m, n
  rmultinom = 2L, # n, p
  rsignrank = 1L # n
)

FUNCTIONS_REWRITE_RF <-
  grep("_rand$", names(FUNCTIONS_STOCHASTIC), invert = TRUE, value = TRUE)

FUNCTIONS_INPLACE <- list(
  rmultinom = list(len = 3L, dest = 4L, type = "int"))

## Here we need to do a bit of a faff because unary functions need
## adding.  This may get tightened up later to either use local() or
## to expand the amount of bits here that a more involved approach is
## required (TODO).
.join <- function(a, b, ...) {
  range1 <- function(x, y) {
    if (x == y) x else range(x, y)
  }
  overlap <- intersect(names(a), names(b))
  if (length(overlap) > 0L) {
    a[overlap] <- lapply(overlap, function(i) range1(a[[i]], b[[i]]))
    ret <- c(a, b[setdiff(names(b), overlap)])
  } else {
    ret <- c(a, b)
  }
  if (length(list(...)) > 0L) {
    ret <- .join(ret, ...)
  }
  ret
}

FUNCTIONS <-
  .join(FUNCTIONS,
        setNames(rep(list(1L), length(FUNCTIONS_UNARY)), FUNCTIONS_UNARY),
        setNames(rep(list(2L), length(FUNCTIONS_INFIX)), FUNCTIONS_INFIX))
rm(.join)

FUNCTIONS_NARY <-
  names(which(vapply(FUNCTIONS, function(x) x[[length(x)]] == Inf, logical(1))))

## This will probab;ly change later, as it should probably be more
## configurable, bit this way we avoid a magic number
DEFAULT_HISTORY_SIZE <- 10000L

## Avoid a lot of error print pasting:
array_dim_name <- function(name, sub = NULL, use = TRUE) {
  if (!is.null(sub)) {
    name <- sprintf("%s_%s", name, sub)
  }
  sprintf("dim_%s", name)
}


initial_name <- function(name) {
  sprintf("initial_%s", name)
}
