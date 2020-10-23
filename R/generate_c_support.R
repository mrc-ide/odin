## This generates non-inclusive ranges; so a full sum will be passed
## in as:
##
## > odin_sum1(x, 0, length(x))
##
## The rewriter generate_c_sexp_sum takes care of converting 'from'
## into a base-0 index
generate_c_support_sum <- function(rank) {
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


generate_c_support_ring <- function(is_package) {
  generate_c_support_external("include/ring/ring.h", "include/ring/ring.c",
                              "ring", is_package)
}


generate_c_support_interpolate <- function(is_package) {
  generate_c_support_external("include/cinterpolate/cinterpolate.h",
                              "include/cinterpolate/cinterpolate.c",
                              "cinterpolate", is_package)
}


## TODO: later on, we should look for LinkingTo within the package
## DESCRIPTION because if it is found then we can do a different,
## probably better, approach.  The net effect on compile time and
## object size will be the same however.
generate_c_support_external <- function(path_h, path_c, package, is_package) {
  filter_includes <- function(filename) {
    x <- readLines(filename)
    x[!grepl("^#include\\s+", x, perl = TRUE)]
  }

  filename_h <- system.file(path_h, package = package, mustWork = TRUE)
  filename_c <- system.file(path_c, package = package, mustWork = TRUE)

  if (is_package) {
    decl <- filter_includes(filename_h)
    defn <- filter_includes(filename_c)
  } else {
    decl <- sprintf('#include "%s"', filename_h)
    defn <- sprintf('#include "%s"', filename_c)
  }

  list(declarations = decl, definitions = defn)
}
