join_library <- function(x) {
  list(declarations = unlist(lapply(x, "[[", "declarations")),
       definitions = unlist(lapply(x, "[[", "definitions")),
       filename = unlist(lapply(x, "[[", "filename")))
}
