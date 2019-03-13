## This generates non-inclusive ranges; so a full sum will be passed
## in as:
##
##   odin_sum1(x, 0, length(x))
##
## The rewriter generate_c_sexp_sum takes care of converting 'from'
## into a base-0 index
generate_c_compiled_sum <- function(rank) {
  i <- seq_len(rank)

  ## TODO: would be nice to avoid use of array_dim_name and INDEX
  ## here, though in general they are not needed as function scope
  ## avoids the worst of things.
  index <- INDEX[i]
  mult <- vcapply(seq_len(rank), function(x)
    array_dim_name("x", paste(seq_len(x - 1), collapse = "")))
  counter <- vcapply(index, strrep, n = 2, USE.NAMES = FALSE)

  limits <- rbind(sprintf_safe("from_%s", index),
                  sprintf_safe("to_%s", index))
  args <- c("x", limits, mult[-1])
  names(args) <- c("double*", rep("int", length(args) - 1))

  loop_open <- sprintf_safe("for (int %s = %s; %s < %s; ++%s) {",
                            index, limits[1, i], index, limits[2, i], index)

  for (j in i) {
    if (j == 1L) {
      loop_body <- sprintf_safe("tot += x[%s + %s];",
                                index[[j]], counter[[j + 1]])
    } else {
      if (j == rank) {
        set_counter <- sprintf_safe("int %s = %s * %s;",
                                    counter[[j]], index[[j]], mult[[j]])
      } else {
        set_counter <- sprintf_safe("int %s = %s * %s + %s;",
                                    counter[[j]], index[[j]],
                                    mult[[j]], counter[[j + 1]])
      }
      loop_body <- c(set_counter, loop)
    }
    loop <- c(loop_open[[j]], paste0("  ", loop_body), "}")
  }

  body <- c("double tot = 0.0;", loop, "return tot;")

  c_function("double",
             sprintf_safe("odin_sum%d", rank),
             args,
             body)
}


generate_c_ring_support <- function(package) {
  ## Annoyingly different from the version used in interpolate
  filter_includes <- function(filename) {
    x <- readLines(filename)
    x[!grepl("^#include\\s+", x, perl = TRUE)]
  }

  r_h <- system.file("include/ring/ring.h", package = "ring", mustWork = TRUE)
  r_c <- system.file("include/ring/ring.c", package = "ring", mustWork = TRUE)
  if (package) {
    decl <- filter_includes(r_h)
    defn <- filter_includes(r_c)
  } else {
    decl <- sprintf('#include "%s"', r_h)
    defn <- sprintf('#include "%s"', r_c)
  }
  list(declarations = decl, definitions = defn)
}


generate_c_interpolate_support <- function() {
  r_h <- system.file("include/cinterpolate/cinterpolate.h",
                     package = "cinterpolate", mustWork = TRUE)
  r_c <- system.file("include/cinterpolate/cinterpolate.c",
                     package = "cinterpolate", mustWork = TRUE)
  decl <- sprintf('#include "%s"', r_h)
  defn <- sprintf('#include "%s"', r_c)
  list(declarations = decl, definitions = defn)
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
