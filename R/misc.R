## Things that should end up elsewhere eventually

join_library <- function(x) {
  list(declarations = unlist(lapply(x, "[[", "declarations")),
       definitions = unlist(lapply(x, "[[", "definitions")),
       filename = unlist(lapply(x, "[[", "filename")))
}


combine_include <- function(x) {
  xx <- unique(unlist(x, FALSE))
  nms <- vcapply(xx, "[[", "name")
  if (any(duplicated(nms))) {
    stop("Duplicated entries in included C support not allowed")
  }
  list(declarations = lapply(xx, "[[", "declaration"),
       definitions = lapply(xx, "[[", "definition"))
}


read_user_c <- function(filename) {
  d <- readLines(filename)

  re1 <- "^[[:alnum:]_*]+ ([[:alnum:]_]+)(.+)"
  i1 <- grep(re1, d)
  i2 <- grep("^}$", d)
  if (length(i1) != length(i2)) {
    stop("Parse error for ", filename)
  }
  name <- sub(re1, "\\1", d[i1])
  defn <- setNames(vcapply(seq_along(i1), function(k)
    paste(d[i1[[k]]:i2[[k]]], collapse = "\n")), name)
  decl <- sub("^([^{]*?)\\s*\\{.*", "\\1;", defn)

  list(declarations = decl, definitions = defn, filename = filename)
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
