generate_js_support_sum <- function(rank) {
  i <- seq_len(rank)

  index <- c("i", "j", "k", "l", "i5", "i6", "i7", "i8")[i]
  mult <- vcapply(seq_len(rank), function(x)
    sprintf("dim%s", paste(seq_len(x - 1), collapse = "")))
  counter <- vcapply(index, strrep, times = 2, USE.NAMES = FALSE)

  limits <- rbind(sprintf("%sFrom", index),
                  sprintf("%sTo", index))
  args <- c("x", limits, mult[-1])

  loop_open <- sprintf("for (var %s = %s; %s < %s; ++%s) {",
                       index, limits[1, i], index, limits[2, i], index)

  for (j in i) {
    if (j == 1L) {
      loop_body <- sprintf("tot += x[%s + %s];",
                           index[[j]], counter[[j + 1]])
    } else {
      if (j == rank) {
        set_counter <- sprintf("var %s = %s * %s;",
                               counter[[j]], index[[j]], mult[[j]])
      } else {
        set_counter <- sprintf("var %s = %s * %s + %s;",
                               counter[[j]], index[[j]],
                               mult[[j]], counter[[j + 1]])
      }
      loop_body <- c(set_counter, loop)
    }
    loop <- c(loop_open[[j]], paste0("  ", loop_body), "}")
  }

  body <- c("var tot = 0.0;", loop, "return tot;")

  js_function(args, body, sprintf("odinSum%d", rank))
}
