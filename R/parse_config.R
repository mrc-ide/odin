## Tidy up the config lines and set some defaults.
##
## There are not many configuration options, and as the horror show
## below indicates, any additional expansoion really requires some
## additional work in the parsing here.
##
## Currently supported:
##
##    base - sets basename (filename and code generation is affected)
##    include - .c files to include
##
## Hopefully all the additional bits can get routed into the
## odin_parse_config_xxx lines.

## TODO: some of this could happily be processed in the parse_expr
## section; as very little depends on more than one line.  Though we
## don't tend to have both the rhs and lhs together through most of
## that.

## Previous note about config scope:
##
##   These all need support on the C side.
##
##   Consider namespacing these here:
##   atol=formals(deSolve::lsoda)$atol,
##   rtol=formals(deSolve::lsoda)$rtol,
##   method="lsoda", # same as deSolve::ode
##   ## delay only
##   mxhist=10000
##
##   Also allow renaming here (time, derivs, etc).
##
## Most of this is worth supporting, and the renaming bits sooner
## rather than later (this lifts the globals from parse.R into an
## extra bit of data to pass around which will infect a lot of the
## code).
odin_parse_config <- function(obj) {
  base <- if (is.null(obj$file)) "odin" else basename_no_ext(obj$file)
  config <- list(base = base, include = NULL)

  is_config <- obj$traits[, "is_config"]
  cfg <- obj$eqs[is_config]
  if (length(cfg) > 0L) {
    ## Filter configuration options out of equations (and traits!)
    obj$eqs <- obj$eqs[!is_config]
    obj$traits <- obj$traits[!is_config, , drop=FALSE]
    obj$names_target <- obj$names_target[!is_config]

    dat <- odin_parse_config_check_types(cfg)

    ## Basic checking that we weren't given an invalid option:
    err <- setdiff(names(dat), names(config))
    if (length(err)) {
      tmp <- dat[names(dat) %in% err]
      odin_error(sprintf("Unknown configuration options: %s",
                         paste(err, collapse=", ")),
                 get_lines(tmp), get_exprs(tmp))
    }

    ## Option specific setting
    ret <- list(
      base = odin_parse_config_base(dat[names(dat) == "base"]),
      include = odin_parse_config_include(dat[names(dat) == "include"],
                                          obj$path))

    config <- modifyList(config, ret[!vlapply(ret, is.null)])
  }

  obj$config <- config
  obj$info$base <- config$base

  obj
}

odin_parse_config_check_types <- function(cfg) {
  f_name <- function(x) {
    x$lhs$name_target
  }
  f_value <- function(x) {
    if (x$rhs$type != "atomic") {
      odin_error("config() rhs must be atomic (not an expression or symbol)",
                 x$line, x$expr)
    }
    list(value=x$rhs$value,
         line=x$line,
         expr=x$expr)
  }

  setNames(lapply(cfg, f_value), vcapply(cfg, f_name))
}

odin_parse_config_base <- function(base) {
  if (length(base) == 0) {
    return(NULL)
  }
  if (length(base) != 1L) {
    stop("Expected a single config(base) option")
  }
  ## Check the basename if given:
  x <- base[[1L]]$value
  if (!is.character(x)) {
    odin_error("config(base) must be a character",
               base[[1L]]$line, base[[1L]]$expr)
  }
  if (!is_c_identifier(x)) {
    odin_error(
      sprintf("Invalid base value: '%s', must be a valid C identifier", x),
      base[[1L]]$line, base[[1L]]$expr)
  }
  x
}

odin_parse_config_include <- function(include, path) {
  if (length(include) == 0) {
    return(NULL)
  }
  err <- !vlapply(include, function(x) is.character(x$value))
  if (any(err)) {
    odin_error("config(include) must be a character",
               get_lines(include[err]), get_exprs(include[err]))
  }

  read1 <- function(i) {
    filename <- file.path(path, include[[i]]$value)
    ok <- file.exists(filename)
    if (any(ok)) {
      filename <- filename[which(ok)[[1L]]]
    } else {
      odin_error(
        sprintf("Could not find file '%s', after looking in:\n%s",
                include[[i]]$value,
                paste(sprintf(" - %s", path), collapse = "\n")),
        include[[i]]$line,
        include[[i]]$expr)
    }
    tryCatch(read_user_c(filename),
             error=function(e)
               odin_error(paste("Could not read include file:", e$message),
                          include[[i]]$line,
                          include[[i]]$expr))
  }
  res <- join_library(lapply(seq_along(include), read1))
  if (any(duplicated(res$declarations))) {
    odin_error("Duplicate declarations while reading includes",
               get_lines(include), get_exprs(include))
  }

  res
}

join_library <- function(x) {
  list(declarations=unlist(lapply(x, "[[", "declarations")),
       definitions=unlist(lapply(x, "[[", "definitions")))
}
