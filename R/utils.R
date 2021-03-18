##' @importFrom stats setNames
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
  env <- new_empty_env()
  env$res <- init
  add <- function(x, ..., literal = FALSE) {
    env$res <- c(env$res,
              if (literal) x else sprintf_safe(x, ...))
  }
  list(add = add,
       length = function(x) length(env$res), # used only in debugging below
       get = function() env$res)
}

collector_list <- function(init = list()) {
  env <- new_empty_env()
  env$res <- init
  list(add = function(x) env$res <- c(env$res, list(x)),
       get = function() env$res)
}

counter <- function() {
  env <- new_empty_env()
  env$n <- 0L
  list(add = function() env$n <- env$n + 1L,
       get = function() env$n,
       reset = function(n) env$n <- 0L)
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

short_hash <- function(x) {
  substr(x, 1L, 8L)
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
    stop(sprintf("%s must be one of {%s}", name,
                 paste(choices, collapse = ", ")),
         call. = FALSE)
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


hash_string <- function(x) {
  digest::digest(charToRaw(x), serialize = FALSE)
}


## Wrappers around jsonlite
to_json <- function(dat, pretty = TRUE) {
  jsonlite::toJSON(dat, null = "null", pretty = pretty, digits = NA)
}


from_json <- function(x) {
  jsonlite::fromJSON(x, simplifyVector = FALSE)
}


read_string <- function(path) {
  readChar(path, file.info(path, extra_cols = FALSE)$size)
}


odin_message <- function(msg, verbose) {
  if (verbose) {
    message(msg)
  }
}


new_empty_env <- function() {
  new.env(parent = emptyenv())
}


odin_file <- function(...) {
  system.file(..., package = "odin", mustWork = TRUE)
}


glue_whisker <- function(template, data) {
  glue::glue(template, .envir = data, .open = "{{", .close = "}}",
             .trim = FALSE)
}


read_lines <- function(path) {
  paste(readLines(path), collapse = "\n")
}


clean_package_name <- function(name) {
  gsub("_", ".", name)
}


na_drop <- function(x) {
  x[!is.na(x)]
}


assert_scalar_logical_or_null <- function(x, name = deparse(substitute(x))) {
  if (!is.null(x)) {
    if (length(x) != 1 || !is.logical(x) || is.na(x)) {
      stop(sprintf("Expected '%s' to be a logical scalar (or NULL)", name))
    }
  }
  invisible(x)
}


assert_scalar_character_or_null <- function(x, name = deparse(substitute(x))) {
  if (!is.null(x)) {
    if (length(x) != 1 || !is.character(x) || is.na(x)) {
      stop(sprintf("Expected '%s' to be a character scalar (or NULL)", name))
    }
  }
  invisible(x)
}


assert_named <- function(x, unique = FALSE, name = deparse(substitute(x))) {
  if (is.null(names(x))) {
    stop(sprintf("'%s' must be named", name), call. = FALSE)
  }
  if (unique && any(duplicated(names(x)))) {
    stop(sprintf("'%s' must have unique names", name), call. = FALSE)
  }
  invisible(x)
}


assert_is <- function(x, what, name = deparse(substitute(x))) {
  if (!inherits(x, what)) {
    stop(sprintf("'%s' must be a %s", name, paste(what, collapse = " / ")),
         call. = FALSE)
  }
  invisible(x)
}
