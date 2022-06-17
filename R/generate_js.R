generate_js <- function(ir, options) {
  dat <- odin_ir_deserialise(ir)

  rewrite <- function(x) {
    generate_js_sexp(x, dat$data, dat$meta)
  }

  features <- vlapply(dat$features, identity)
  supported <- c("continuous",
                 "initial_time_dependent", "has_array", "has_user",
                 "has_output", "has_interpolate", "discrete", "has_stochastic")
  unsupported <- setdiff(names(features)[features], supported)
  if (length(unsupported) > 0L) {
    stop("Using unsupported features: ",
         paste(squote(unsupported), collapse = ", "))
  }

  eqs <- generate_js_equations(dat, rewrite)
  core <- generate_js_core(eqs, dat, rewrite)

  fns <- unlist(lapply(dat$equations, function(x) x$depends$functions),
                FALSE, FALSE)
  uses_sum <- any(c("odin_sum", "sum") %in% fns)

  ## This is all we need to dump out
  list(code = generate_js_generator(core, dat),
       name = dat$config$base,
       ir = ir,
       features = dat$features,
       include = c(interpolate.js = dat$features$has_interpolate,
                   random.js = dat$features$has_stochastic,
                   discrete.js = dat$features$discrete,
                   support_sum.js = uses_sum))
}


generate_js_core <- function(eqs, dat, rewrite) {
  core <- list(
    create = generate_js_core_create(eqs, dat, rewrite),
    set_user = generate_js_core_set_user(eqs, dat, rewrite),
    rhs_eval = generate_js_core_rhs_eval(eqs, dat, rewrite),
    run = generate_js_core_run(eqs, dat, rewrite),
    output = generate_js_core_output(eqs, dat, rewrite),
    metadata = generate_js_core_metadata(eqs, dat, rewrite),
    coef = generate_js_coef(eqs, dat, rewrite),
    initial_conditions = generate_js_core_initial_conditions(
      eqs, dat, rewrite))
  if (dat$features$discrete) {
    core$rhs <- generate_js_core_update(eqs, dat, rewrite)
  } else {
    core$rhs <- generate_js_core_deriv(eqs, dat, rewrite)
  }
  core
}


generate_js_core_create <- function(eqs, dat, rewrite) {
  body <- collector()
  body$add("this.%s = {};", dat$meta$internal)
  body$add("var %s = this.%s;", dat$meta$internal, dat$meta$internal)
  body$add(js_flatten_eqs(eqs[dat$components$create$equations]))
  body$add("this.setUser(%s, unusedUserAction);", dat$meta$user)
  args <- c(dat$meta$user, "unusedUserAction")
  js_function(args, body$get(), dat$config$base)
}


generate_js_core_set_user <- function(eqs, dat, rewrite) {
  update_metadata <- "this.updateMetadata();"
  allowed <- paste(dquote(names(dat$user)), collapse = ", ")
  check_user <- sprintf("checkUser(%s, [%s], unusedUserAction);",
                        dat$meta$user, allowed)
  if (dat$features$has_user) {
    body <- c(
      check_user,
      sprintf("var %s = this.%s;", dat$meta$internal, dat$meta$internal),
      js_flatten_eqs(eqs[dat$components$user$equations]),
      update_metadata)
  } else {
    body <- c(check_user, update_metadata)
  }
  args <- c(dat$meta$user, "unusedUserAction")
  js_function(args, body)
}


generate_js_core_deriv <- function(eqs, dat, rewrite) {
  variables <- dat$components$rhs$variables
  equations <- dat$components$rhs$equations

  internal <- sprintf("var %s = this.%s;", dat$meta$internal, dat$meta$internal)
  unpack <- lapply(variables, js_unpack_variable, dat, dat$meta$state, rewrite)

  body <- js_flatten_eqs(c(internal, unpack, eqs[equations]))

  args <- c(dat$meta$time, dat$meta$state, dat$meta$result)
  js_function(args, body)
}


generate_js_core_update <- function(eqs, dat, rewrite) {
  variables <- union(dat$components$rhs$variables,
                     dat$components$output$variables)
  equations <- union(dat$components$rhs$equations,
                     dat$components$output$equations)

  internal <- sprintf("var %s = this.%s;", dat$meta$internal, dat$meta$internal)
  unpack <- lapply(variables, js_unpack_variable, dat, dat$meta$state, rewrite)
  body <- js_flatten_eqs(c(internal, unpack, eqs[equations]))

  args <- c(dat$meta$time, dat$meta$state, dat$meta$result, dat$meta$output)
  js_function(args, body)
}


generate_js_core_output <- function(eqs, dat, rewrite) {
  if (!dat$features$has_output) {
    return(NULL)
  }

  variables <- dat$components$output$variables
  equations <- dat$components$output$equations

  internal <- sprintf("var %s = this.%s;", dat$meta$internal, dat$meta$internal)
  alloc <- sprintf("var %s = new Array(%s);",
                   dat$meta$output, rewrite(dat$data$output$length))
  unpack <- lapply(variables, js_unpack_variable, dat, dat$meta$state, rewrite)
  ret <- sprintf("return %s;", dat$meta$output)
  body <- js_flatten_eqs(c(internal, alloc, unpack, eqs[equations], ret))

  args <- c(dat$meta$time, dat$meta$state)
  js_function(args, body)
}


generate_js_core_run <- function(eqs, dat, rewrite) {
  if (dat$features$discrete) {
    args <- c("times", "y0")
    body <- sprintf("return iterateOdin(this, times, y0, %s);",
                    rewrite(dat$data$output$length))
  } else {
    args <- c("times", "y0", "control")
    body <-
      "return integrateOdin(this, times, y0, control);"
  }
  js_function(args, body)
}


generate_js_coef <- function(eqs, dat, rewrite) {
  if (!dat$features$has_user) {
    return("{}")
  }
  js_dict <- function(x) {
    sprintf("{%s}", paste(sprintf("%s: %s", names(x), x), collapse = ", "))
  }
  f <- function(x) {
    data_info <- dat$data$elements[[x$name]]
    if (is.null(x$user$default)) {
      default <- "null"
    } else {
      default <- as.character(rewrite(x$user$default))
    }
    d <- c(has_default = tolower(is.null(x$user$default)),
           default = default,
           rank = as.character(data_info$rank),
           min = as.character(x$user$min %||% "-Infinity"),
           max = as.character(x$user$max %||% "Infinity"),
           integer = tolower(data_info$storage_type == "integer"))
    js_dict(d)
  }
  js_dict(vcapply(dat$equations[names(dat$user)], f))
}


generate_js_core_metadata <- function(eqs, dat, rewrite) {
  body <- c("this.metadata = {};",
            "var internal = this.internal;")
  if (dat$features$has_array) {
    variables <- names(dat$data$variable$contents)
    output <- names(dat$data$output$contents)
    contents <- dat$data$elements[c(variables, output)]

    add_name <- function(el) {
      if (el$rank == 0) {
        ret <- sprintf('this.metadata.ynames.push("%s");', el$name)
      } else if (el$rank == 1) {
        len <- rewrite(el$dimnames$length)
        ret <- c(
          sprintf("for (var i = 1; i <= %s; ++i) {", len),
          sprintf('  this.metadata.ynames.push("%s[" + i + "]");', el$name),
          sprintf("}"))
      } else {
        index <- paste0("i", seq_len(el$rank))
        pos <- paste(index, collapse = ' + "," + ')
        ret <- sprintf('this.metadata.ynames.push("%s[" + %s + "]");',
                           el$name, pos)
        for (i in seq_len(el$rank)) {
          len <- rewrite(el$dimnames$dim[[i]])
          loop <- sprintf("for (var %s = 1; %s <= %s; ++%s) {",
                          index[[i]], index[[i]], len, index[[i]])
          ret <- c(loop, paste0("  ", ret), "}")
        }
      }
      ret
    }
    ynames <- c(sprintf('this.metadata.ynames = ["%s"];', dat$meta$time),
                js_flatten_eqs(lapply(contents, add_name)))
    body <- c(body, ynames)
  } else {
    ynames <- c(dat$meta$time,
                names(dat$data$variable$contents),
                names(dat$data$output$contents))
    body <- c(body,
              sprintf("this.metadata.ynames = [%s];",
                      paste(dquote(ynames), collapse = ", ")))
  }

  if (dat$features$has_interpolate) {
    args_min <- js_fold_call("Math.max",
                             vcapply(dat$interpolate$min, function(x)
                               sprintf("%s[0]", rewrite(x))))
    if (length(dat$interpolate$max) == 0) {
      args_max <- "Infinity"
    } else {
      args_max <- js_fold_call(
        "Math.min",
        vcapply(dat$interpolate$max, function(x)
          sprintf("%s[%s - 1]", rewrite(x),
                  rewrite(dat$data$elements[[x]]$dimnames$length))))
    }
    body <- c(
      body,
      "this.metadata.interpolateTimes = {",
      sprintf("  min: %s,", args_min),
      sprintf("  max: %s", args_max),
      "};")
  } else {
    body <- c(
      body,
      "this.metadata.interpolateTimes = null;")
  }

  len_block <- function(location) {
    if (location == "internal") {
      ## This excludes interpolate_data and ring_buffer
      keep <- vlapply(dat$data$elements, function(x)
        x$location == "internal" &&
        x$storage_type %in% c("double", "int", "bool"))
      contents <- dat$data$elements[keep]
    } else {
      contents <- dat$data$elements[names(dat$data[[location]]$contents)]
    }
    if (length(contents) == 0) {
      sprintf("this.metadata.%sOrder = null;", location)
    } else {
      len <- vcapply(contents, generate_js_dim, rewrite)
      sprintf("this.metadata.%sOrder = {\n  %s\n};",
              location, paste(len, collapse = ",\n  "))
    }
  }

  body <- c(body,
            len_block("internal"),
            len_block("variable"),
            len_block("output"))

  js_function(NULL, body)
}


## This one is just a helper - not sure if it's optimally structured.
generate_js_core_rhs_eval <- function(eqs, dat, rewrite) {
  args <- c(dat$meta$time, dat$meta$state)

  if (dat$features$discrete && dat$features$has_output) {
    body <- c(
      sprintf("var %s = zeros(%s.length);",
              dat$meta$result, dat$meta$state),
      sprintf("var %s = zeros(%s);",
              dat$meta$output, rewrite(dat$data$output$length)),
      sprintf("this.rhs(%s, %s, %s, %s);",
              dat$meta$time, dat$meta$state, dat$meta$result, dat$meta$output),
      sprintf("return %s.concat(%s);", dat$meta$result, dat$meta$output))
  } else {
    if (dat$features$has_output) {
      output <- sprintf("%s = %s.concat(this.output(%s, %s));",
                        dat$meta$result, dat$meta$result, dat$meta$time,
                        dat$meta$state)
    } else {
      output <- NULL
    }
    body <- c(
      sprintf("var %s = zeros(%s.length);", dat$meta$result, dat$meta$state),
      sprintf("this.rhs(%s, %s, %s);",
              dat$meta$time, dat$meta$state, dat$meta$result),
      output,
      sprintf("return %s;", dat$meta$result))
  }

  js_function(args, body)
}


generate_js_core_initial_conditions <- function(eqs, dat, rewrite) {
  set_initial <- function(el) {
    data_info <- dat$data$elements[[el$name]]
    if (data_info$rank == 0L) {
      lhs <- sprintf("%s[%s]", dat$meta$state, rewrite(el$offset))
      sprintf("%s = %s.%s;", lhs, dat$meta$internal, el$initial)
    } else {
      c(sprintf("for (var i = 0; i < %s; ++i) {",
                rewrite(data_info$dimnames$length)),
        sprintf("  %s[%s + i] = %s.%s[i];",
                dat$meta$state, rewrite(el$offset),
                dat$meta$internal, el$initial),
        "}")
    }
  }

  internal <- sprintf("var %s = this.%s;",
                      dat$meta$internal, dat$meta$internal)
  if (length(dat$components$initial$equations) == 0) {
    eqs_initial <- NULL
  } else {
    subs <- lapply(dat$data$variable$contents, function(x) rewrite(x$initial))
    eqs_initial <- dat$equations[dat$components$initial$equations]
    eqs_initial <- lapply(ir_substitute(eqs_initial, subs),
                          generate_js_equation, dat, rewrite)
  }

  initial <- js_flatten_eqs(lapply(dat$data$variable$contents, set_initial))

  body <- collector()
  body$add(internal)
  body$add(js_flatten_eqs(eqs_initial))
  body$add("var %s = zeros(%s);",
           dat$meta$state, rewrite(dat$data$variable$length))
  body$add(initial)
  body$add("return %s;", dat$meta$state)

  args <- dat$meta$time
  js_function(args, body$get())
}


generate_js_generator <- function(core, dat) {
  base <- dat$config$base
  method <- function(name, x) {
    n <- length(x)
    x[[1]] <- sprintf("%s.prototype.%s = %s", base, name, x[[1]])
    x[[n]] <- paste0(x[[n]], ";")
    x
  }
  field <- function(name, x) {
    stopifnot(length(x) == 1)
    sprintf("%s.prototype.%s = %s;", base, name, x)
  }

  body <- collector()
  body$add(core$create)
  body$add(method("setUser", core$set_user))
  body$add(method("rhs", core$rhs))
  if (!is.null(core$output)) {
    body$add(method("output", core$output))
  }
  body$add(field("interpolateTime", "null"))
  body$add(method("rhsEval", core$rhs_eval))
  body$add(method("initial", core$initial_conditions))
  body$add(method("run", core$run))
  body$add(field("coef", core$coef))
  body$add(method("updateMetadata", core$metadata))
  body$add("return %s;", base)

  c(sprintf("%s.%s = (function() {", JS_GENERATORS, base),
    paste0("  ", body$get()),
    "}());")
}


generate_js_dim <- function(data_info, rewrite) {
  if (data_info$rank == 0L) {
    len <- "null"
  } else if (data_info$rank == 1L) {
    len <- rewrite(data_info$dimnames$length)
  } else {
    len <- sprintf(
      "[%s]", paste(vcapply(data_info$dimnames$dim, rewrite), collapse = ", "))
  }
  sprintf('"%s": %s', data_info$name, len)
}
