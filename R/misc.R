## Things that should end up elsewhere eventually

join_library <- function(x) {
  list(declarations = unlist(lapply(x, "[[", "declarations")),
       definitions = unlist(lapply(x, "[[", "definitions")),
       filename = unlist(lapply(x, "[[", "filename")))
}


combine_include <- function(x) {
  declarations <- unlist(lapply(x, "[[", "declarations"), FALSE)
  definitions <- unlist(lapply(x, "[[", "definitions"), FALSE)

  check <- function(x) {
    dups <- unique(names(x)[duplicated(names(x))])
    for (nm in dups) {
      if (length(unique(x[names(x) == nm])) > 1) {
        stop(sprintf(
          "Duplicated entries in included C support not allowed (check '%s')",
          nm))
      }
    }
    unique(x)
  }

  list(declarations = check(declarations),
       definitions = check(definitions))
}


read_user_c <- function(filename) {
  read_include_c(filename)$data
}


read_include_c <- function(filename) {
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

  list(
    names = name,
    data = list(names = name,
                declarations = decl,
                definitions = defn,
                filename = filename))
}


read_include_r <- function(filename) {
  e <- new.env(parent = baseenv())
  sys.source(filename, e)
  list(names = names(e),
       data = list(source = readLines(filename)))
}


read_include_unsupported <- function(target) {
  force(target)
  function(filename) {
    stop(sprintf("'config(include)' is not supported for target '%s'", target))
  }
}


is_c_identifier <- function(x) {
  grepl("^[A-Za-z_][A-Za-z0-9_]*", x) & !(x %in% RESERVED_C)
}


is_dim_or_length <- function(x) {
  is_call(x, quote(dim)) || is_call(x, quote(length))
}
