odin_parse_prepare <- function(exprs) {
  expr_is_assignment <- function(x) {
    length(x) == 3L &&
      (identical(x[[1]], quote(`<-`)) || identical(x[[1]], quote(`=`)))
  }
  ## First pass is to check that all operations are assignments.  No
  ## for loops, no if/else statements.
  err <- which(!vlapply(exprs, expr_is_assignment))
  if (length(err) > 0L) {
    odin_error("Every line must contain an assignment", err, exprs[err])
  }

  exprs
}


## We're going to need to wrap this up like testthat I think, so that
## we can catch these and group them together.  But leaving that for
## now.
odin_error <- function(msg, line, expr) {
  ret <- odin_info_data(msg, line, expr, "error")
  class(ret) <- c("odin_error", "error", "condition")
  stop(ret)
}


odin_info_data <- function(msg, line, expr, type) {
  if (is.expression(expr)) {
    expr_str <- vcapply(expr, deparse_str)
  } else {
    expr_str <- deparse_str(expr)
  }
  str <- odin_info_expr(line, expr_str)
  list(message = paste0(msg, paste0("\n\t", str, collapse = "")),
       msg = msg,
       line = line,
       expr = expr,
       type = type)
}

odin_info_expr <- function(line, expr_str) {
  sprintf(ifelse(is.na(line), "%s", "%s # (line %s)"), expr_str, line)
}


recursive_dependencies <- function(order, deps, vars) {
  deps_rec <- setNames(vector("list", length(order)), order)
  for (i in order) {
    j <- as.character(unlist(deps[i]))
    deps_rec[[i]] <-
      c(j, unique(as.character(unlist(deps_rec[j], use.names = FALSE))))
  }
  deps_rec
}


is_c_identifier <- function(x) {
  ## Keyword list from:
  ## http://en.cppreference.com/w/c/keyword
  c_reserved <-
    c("auto", "break", "case", "char", "const", "continue", "default",
      "do", "double", "else", "enum", "extern", "float", "for", "goto",
      "if", "inline", "int", "long", "register", "restrict", "return",
      "short", "signed", "sizeof", "static", "struct", "switch", "typedef",
      "union", "unsigned", "void", "volatile", "while")
  grepl("^[A-Za-z_][A-Za-z0-9_]*", x) & !(x %in% c_reserved)
}

is_dim_or_length <- function(x) {
  is_call(x, quote(dim)) || is_call(x, quote(length))
}
