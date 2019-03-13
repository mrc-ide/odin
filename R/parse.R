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
