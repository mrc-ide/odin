##' @importFrom stats na.omit setNames
##' @importFrom utils modifyList
vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
viapply <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
}
vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

## Like deparse() but always produce a single string
deparse_str <- function(x) {
  paste(deparse(x), collapse = "\n")
}

collector <- function(init = character(0)) {
  res <- init
  add <- function(x, ..., literal = FALSE) {
    res <<- c(res,
              if (literal) x else sprintf_safe(x, ...))
  }
  list(add = add,
       length = function(x) length(res), # used only in debugging below
       get = function() res)
}

collector_list <- function(init = list()) {
  res <- init
  list(add = function(x) res <<- c(res, list(x)),
       get = function() res)
}

pastec <- function(..., collapse = ", ") {
  paste(..., collapse = collapse)
}

strrep <- function(n, x = " ") {
  paste(rep(x, n), collapse = "")
}

is_integer_like <- function(x, tol = sqrt(.Machine$double.eps)) {
  is.integer(x) || (is.numeric(x) && all(abs(x - round(x)) < tol))
}

is_call <- function(expr, symbol) {
  if (is.character(symbol)) {
    symbol <- as.name(symbol)
  }
  is.recursive(expr) && identical(expr[[1L]], symbol)
}

is_directory <- function(path) {
  file.exists(path) && file.info(path, extra_cols = FALSE)$isdir
}

basename_no_ext <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

substitute_ <- function(expr, env) {
  eval(substitute(substitute(y, env), list(y = expr)))
}

expand_grid_int <- function(x) {
  if (length(x) == 1L) {
    matrix(seq_len(x), x, 1L)
  } else if (length(x) >= 2L) {
    unname(as.matrix(do.call("expand.grid", lapply(x, seq_len), quote = TRUE)))
  }
}

is_duplicated <- function(x) {
  x %in% x[duplicated(x)]
}

names_if <- function(x) {
  names(x)[x]
}

## Abstract the hashing away in case we go for something like openssl.
hash_object <- function(object) {
  digest::digest(object)
}


hash_files <- function(filenames, named = FALSE) {
  if (length(filenames) == 0L) {
    return(NULL)
  }
  hash <- tools::md5sum(filenames)
  if (any(is.na(hash))) {
    stop("Files missing")
  }
  if (named) hash else unname(hash)
}

short_hash <- function(x) {
  substr(x, 1L, 8L)
}

## This is going to be used to keep track of dlls that we load
.dlls <- collector()

dyn_load <- function(dll) {
  dll_full <- normalizePath(dll, mustWork = TRUE)
  dyn.load(dll_full)
  .dlls$add(dll_full)
}

dllname <- function(base) {
  paste0(base, .Platform$dynlib.ext)
}

dquote <- function(x) {
  sprintf('"%s"', x)
}

squote <- function(x) {
  sprintf("'%s'", x)
}

odin_version <- function() {
  list(odin = utils::packageVersion("odin"),
       cinterpolate = utils::packageVersion("cinterpolate"),
       r = getRversion(),
       platform = version$platform)
}


match_value <- function(x, choices, name = deparse(substitute(x))) {
  stopifnot(length(x) == 1L, is.character(x), !is.na(x))
  i <- match(x, choices)
  if (is.na(i)) {
    stop(sprintf("%s must be one of {%s}", name, paste(choices, collapse=", ")),
         call.=FALSE)
  }
  choices[[i]]
}


adrop <- function(x, i) {
  d <- dim(x)
  ok <- all(d[i] == 1L)
  if (!ok) {
    stop("Can't drop selected dimensions")
  }
  dim(x) <- d[if (is.logical(i)) !i else -i]
  x
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}


scalar <- function(x) {
  jsonlite::unbox(x)
}


drop_null <- function(x) {
  x[!vlapply(x, is.null)]
}


list_to_character <- function(x) {
  vcapply(x, identity)
}


sort_list <- function(x) {
  x[order(names(x))]
}


substitute_ <- function(expr, env) {
  eval(substitute(substitute(y, env), list(y = expr)))
}


as_function <- function(args, body, env) {
  as.function(c(args, body), env)
}


sprintf_safe <- function(fmt, ...) {
  dots <- list(...)
  if (any(vlapply(dots, is.null))) {
    stop("Passed empty format parameter to formatter")
  }
  if (length(dots) == 0) {
    fmt
  } else {
    sprintf(fmt, ...)
  }
}
