## Coming out of this I really want to have all the validation done,
## so there's quite a bit of analysis that will be required here?

TYPE_CONSTANT <- 1L
TYPE_USER <- 2L
TYPE_TIME <- 3L
TYPES <- c("constant", "user", "time")
TIME_VARIABLE <- "time"

odin_process <- function(dat) {
  ## Pull out the
  deps_vars <- lapply(dat$vars$rhs, function(x) x$depends$variables)
  deps_initial <- lapply(dat$initial$rhs, function(x) x$depends$variables)
  deps_deriv <- lapply(dat$deriv$rhs, function(x) x$depends$variables)

  res <- process_variables(dat)
  res <- process_initial(res)
  res <- process_derivs(res)

  ## Some checking:
  if (length(res$deriv$depends$user) > 0L) stop("Not yet handled")
  ## if (length(res$deriv$depends$time) > 0L) stop("Not yet handled")

  ## Next, work out what depends on what; that's what the underlying
  ## structure is going to need.

  ## There are two passes through here; there's the initial
  ## calculation ordering and the derivative calculation.  They're
  ## allowed to be different.
  ##
  ## names(deps_deriv) <- sprintf("deriv(%s)", names(deps_deriv))
  ## ord_initial <- topological_order(c(deps_vars, deps_initial))
  ## ord_deriv <- topological_order(c(deps_vars, deps_deriv))

  res
}

## TODO: check all functions are going to work ok.
process_variables <- function(dat) {
  lhs <- dat$vars$lhs
  rhs <- dat$vars$rhs

  ## Types of dependency classification (which are contageous)
  ##   1. Constant -- set during compilation
  ##   2. User     -- set during initialisation
  ##   3. Time     -- set during derivative calculation
  ##               -- includes calculations that are dependent on the
  ##                  state variables, as they depend on time too.
  ##
  ## I think there are two sorts of time things to consider; one is
  ## dependent on a time-changing variable (implicit time via y(t))
  ## the other is as a function of time ~ f(t)).
  ##
  ## The reason why these are different is that initial conditions
  ## should be computed differently if they vary in the two ways.

  ## TODO: The variable used for time needs to be user-specifiable and
  ## then it needs checking that it is not on any lhs.
  initial <- unique(names(dat$initial$lhs))
  types <- c(setNames(rep_len(NA_integer_, length(lhs)), names(lhs)),
             setNames(rep_len(TYPE_TIME, length(initial)), initial),
             setNames(TYPE_TIME, TIME_VARIABLE))

  dat$vars$envir <- new.env(parent=.GlobalEnv)
  dat$vars$types <- process_eval_loop(lhs, rhs, types, dat$vars$envir)

  dat
}

process_initial <- function(dat) {
  ## At this point, we hope to be able to resolve the initial
  ## conditions.  They can be processed if they vary with other
  ## variables only; but not if they vary with user input; then we'd
  ## have to defer processing.

  ## At this point we need to start assembling the memory for the
  ## initial conditions; we need to know how much there is and which
  ## variables are stored where.

  ## One option in C will be to do:

  ## double *y;
  ## struct pars {
  ##   double *y;
  ##   const double * v1;
  ## }
  ##
  ## And arrange for the memory locations to be copied over.
  ##
  ## The initial conditions might depend on time-dependent variables
  ## that explicitly involve time; these must be initialised before
  ## this is evaluated; that's tricky because we end up in a situation
  dat$initial$envir <- new.env(parent=dat$vars$envir)
  dat$initial$types <- process_eval_loop(dat$initial$lhs, dat$initial$rhs,
                                         dat$vars$types, dat$initial$envir)

  ## TODO: Need to do the same thing as derivs here but for the
  ## initial conditions; these get split into constant/user/time
  ## again.

  ## TODO: Not really sure what to do about time dependent initial
  ## conditions.  It should be possible (and is kind of implied if two
  ## initial conditions vary with one another).  It falls out of the
  ## deSolve scope where initial conditions come from nowhere.  So
  ## we're going to return an object that self initialises rather than
  ## the usual deSolve thing.  But perhaps if initial() is missing
  ## that defaults to being user specified?  In _that_ case we need to
  ## decide in the *order* of parameters.  In any case we're looking
  ## at some wrappers around deSolve.  But that's not that bad because
  ## that frees us to try some different types of solutions too.
  ##
  ## The issue is that we'd need to know what the initial time is and
  ## we _cannot_ assume that is zero.  So this needs to be done around
  ## initmod?
  ##
  ## TODO: There will be a lot of validation here to check that the
  ## whole state space is occupied.
  ##
  ## TODO: How do we decide on the appropriate order for the state
  ## space?
  ##
  ## TODO: Similar issues turn up for the derivatives calculations; do
  ## we try and work out which are zero?  Do we try and remove them
  ## from the calculation?
  ##
  ## TODO: With the derivatives calculations, is it worth trying to
  ## allow specifying that some of the calculations are matrix
  ## multiplications?  That has the potential to allow parallelisation
  ## and avoiding some cache thrashing.  But I don't really see how it
  ## would be easy to both declaratively express parameters _and_
  ## express that some can be seen as a matrix.  It might be worth a
  ## look though (something like
  ##   matrix(QP) <- P[na, na] # think of P as an na x na matrix
  ##   matrix(QP) <- P[,,1] # think of this slice of P as a matrix?
  ##   deriv(P) <- QP %*% v + something_else

  ## TODO: These ones are different because we'd only track down deps
  ## for non-constants, I think.  There's a self-dependency issue here
  ## for array cases.
  deps <-
    setdiff(unique(unlist(lapply(dat$initial$rhs,
                                 function(x) x$depends$variables),
                          use.names=FALSE)), names(dat$initial$lhs))
  self <- names(dat$initial$lhs)
  deps_vars <- lapply(dat$initial$rhs[c(self, deps)],
                      function(x) x$depends$variables)
  deps <- topological_order(
    deps_vars[recursive_dependencies(union(self, deps), deps_vars)])
  deps <- process_split_stage(dat$initial$types[deps])

  deps$constant <-
    setNames(lapply(deps$constant, get, dat$initial$envir), deps$constant)
  dat$initial$depends <- deps

  dat
}

process_derivs <- function(dat) {
  ## Compute the dependencies of the derivative calculations:
  deps <-
    setdiff(unique(unlist(lapply(dat$deriv$rhs,
                                 function(x) x$depends$variables),
                          use.names=FALSE)), names(dat$deriv$lhs))

  ## Options here:
  ## 1: constant
  ##    -- these are a set of constants
  ## 2: user:
  ##   -- these are a variable set of parameters
  ## 3: time:
  ##   -- these will be set in the derivative calculations

  ## Need to do a topological sort
  ## here:
  deps_vars <- lapply(dat$vars$rhs[deps], function(x) x$depends$variables)
  deps <- topological_order(deps_vars[recursive_dependencies(deps, deps_vars)])
  deps <- process_split_stage(dat$vars$types[deps])

  ## Resolve the constants:
  ##
  ## TODO: How do the bits here and initial interact with each other?
  ## No point fetching the same constant twice.
  ##
  ##   -- not entirely sure what I meant there?  I think that I mean
  ##     that some constants will be used in the initialisation and
  ##     the derivative calculation so we need to be careful where
  ##     they are put.
  deps$constant <-
    setNames(lapply(deps$constant, get, dat$vars$envir), deps$constant)
  dat$deriv$depends <- deps

  ## dat$deriv$envir <- new.env(parent=dat$initial$envir)

  ## ## Here, I think we need to establish the appropriate order for the
  ## ## differential equations; it might be different to the order for
  ## ## the initial conditions. However, the names of the derivatives are
  ## ## a bit weird.  For now I am punting on it and doing this manually.

  ## ## What I need to do here is redeclare a *second* set of the state
  ## ## variables that declare on nothing.
  ## lhs <- dat$deriv$lhs
  ## rhs <- dat$deriv$rhs
  ## nms <- names(lhs)
  ## ## TODO: Do this in the initial processing.
  ## names(lhs) <- names(rhs) <- sprintf("deriv(%s)", nms)

  ## deps <- lapply(rhs, function(x) x$depends$variables)

  ## ord <- unique(topological_order(deps))


  ## ## dat$deriv$types <- process_eval_loop(,
  ## ##                                      dat$vars$types, dat$deriv$envir)

  ## if (names(dat$deriv$envir) > 0L) {
  ##   stop("Constant ODE's detected")
  ## }

  dat
}

check_type <- function(rhs, types) {
  ## TODO: Allow for some functions to trigger time dependence
  ## perhaps?
  deps <- rhs$depends$variables
  if (length(deps) > 0L) {
    unk <- setdiff(deps, names(types))
    if (length(unk) > 0L) {
      ## TODO: Better reporting here; needs keeping line numbers
      ## around; hard to do for arrays.
      stop("Undefined reference to varaiable %s",
           paste(unk, collapse=", "))
    }
    max(types[deps])
  } else {
    TYPE_CONSTANT
  }
}

process_eval_loop <- function(lhs, rhs, types, envir, order=TRUE) {
  if (order) {
    deps <- lapply(rhs, function(x) x$depends$variables)
    ord <- unique(topological_order(deps))
  } else {
    deps <- lapply(rhs, function(x) x$depends$variables)
  }

  for (i in ord) {
    lhs_i <- lhs[[i]]
    rhs_i <- rhs[[i]]

    if (lhs_i$type == "symbol") {
      ## TODO: this is going to turn some arrays into arrays; be
      ## really careful there; an array assignment needs to create new
      ## array assignments; I think if the rhs is an array we should
      ## throw, but eventually we need to allow element extraction
      ## here.
      if (rhs_i$type == "atomic") {
        assign(i, rhs_i$value, envir)
        types[[i]] <- TYPE_CONSTANT
      } else { # expression
        t <- check_type(rhs_i, types)
        if (t == TYPE_CONSTANT) {
          ## TODO: This misses any local functions that need to be
          ## defined.  They'd want to go first and be picked up by the
          ## code scanning code.
          ##
          ## TODO: If we're compiling a more standalone model, would
          ## this move into C?
          ##
          ## TODO: I don't think this will naturally deal that well
          ## with array access.
          assign(i, eval(rhs_i$value, envir), envir)
        }
        types[[i]] <- t
      }
    } else { # arrays are not done yet
      ## TODO: For array calculations, it will be necessary that a
      ## *mix* of these are used (some could be most elements are
      ## constant but some are set):
      ##   variable[] <- 0
      ##   variable[1] <- another[1]
      stop("not handled yet")
    }
  }

  types
}

process_split_stage <- function(x) {
  split(names(x), factor(TYPES[x], TYPES))
}

recursive_dependencies <- function(x, deps) {
  ret <- character(0)
  while (length(x) > 0L) {
    ret <- c(ret, x)
    x <- setdiff(unlist(deps[x]), ret)
  }
  ret
}
