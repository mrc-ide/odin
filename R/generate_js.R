generate_js <- function(ir, options) {
  dat <- odin_ir_deserialise(ir)

  if (dat$features$mixed) {
    stop("Models that mix deriv() and update() are not supported")
  }

  rewrite <- function(x) {
    generate_js_sexp(x, dat$data, dat$meta)
  }

  features <- vlapply(dat$features, identity)
  supported <- c("continuous", "discrete", "has_stochastic",
                 "initial_time_dependent", "has_array", "has_user",
                 "has_output", "has_delay", "has_interpolate")
  unsupported <- setdiff(names(features)[features], supported)
  if (length(unsupported) > 0L) {
    stop("Using unsupported features: ",
         paste(squote(unsupported), collapse = ", "))
  }
  if (features[["discrete"]] && features[["has_output"]]) {
    stop("Using unsupported features: 'has_output'")
  }

  eqs <- generate_js_equations(dat, rewrite)
  core <- generate_js_core(eqs, dat, rewrite)

  keep <- vlapply(dat$data$elements, function(x)
    x$location == "internal" &&
    x$storage_type %in% c("double", "int", "bool") &&
    x$rank > 1)
  internal_dim <- lapply(dat$data$elements[keep], function(x) x$dimnames$dim)

  ## This is all we need to dump out
  list(code = generate_js_generator(core, dat),
       name = dat$config$base,
       internal_dim = internal_dim,
       ir = ir,
       features = dat$features)
}


generate_js_core <- function(eqs, dat, rewrite) {
  core <- list(
    create = generate_js_core_create(eqs, dat, rewrite),
    set_user = generate_js_core_set_user(eqs, dat, rewrite),
    initial_conditions = generate_js_core_initial_conditions(eqs, dat, rewrite),
    get_internal = generate_js_core_get_internal())

  if (dat$features$discrete) {
    core$update <- generate_js_core_update(eqs, dat, rewrite)
    core$info <- generate_js_core_info(eqs, dat, rewrite)
    core$size <- generate_js_core_size(eqs, dat, rewrite)
  } else {
    core$rhs <- generate_js_core_deriv(eqs, dat, rewrite)
    core$output <- generate_js_core_output(eqs, dat, rewrite)
    core$names <- generate_js_core_names()
    core$update_metadata <- generate_js_core_update_metadata(eqs, dat, rewrite)
    core$get_metadata <- generate_js_core_get_metadata()
  }
  core
}


generate_js_core_create <- function(eqs, dat, rewrite) {
  body <- collector()
  body$add("this.base = base;")
  body$add("this.%s = {};", dat$meta$internal)
  body$add("var %s = this.%s;", dat$meta$internal, dat$meta$internal)
  body$add(js_flatten_eqs(eqs[dat$components$create$equations]))
  body$add("this.setUser(%s, unusedUserAction);", dat$meta$user)
  if (dat$features$has_delay && !dat$features$discrete) {
    body$add("this.%s = NaN;", rewrite(dat$meta$initial_time))
  }
  args <- c("base", dat$meta$user, "unusedUserAction")
  js_function(args, body$get(), "constructor")
}


generate_js_core_set_user <- function(eqs, dat, rewrite) {
  is_discrete <- dat$features$discrete

  update_metadata <- "this.updateMetadata();"
  allowed <- paste(dquote(names(dat$user)), collapse = ", ")
  check_user <- sprintf("this.base.user.checkUser(%s, [%s], unusedUserAction);",
                        dat$meta$user, allowed)

  body <- collector()
  body$add(check_user)

  if (dat$features$has_user) {
    ## TODO: lots of opportunities to use const here
    body$add("const %s = this.%s;", dat$meta$internal, dat$meta$internal)
    body$add(js_flatten_eqs(eqs[dat$components$user$equations]))
  }

  ## This bit we only need to do for continuous models, and won't need
  ## to do in practice.
  if (!is_discrete) {
    body$add(update_metadata)
  }

  args <- c(dat$meta$user, "unusedUserAction")
  js_function(args, body$get())
}


generate_js_core_deriv <- function(eqs, dat, rewrite) {
  variables <- dat$components$rhs$variables
  equations <- dat$components$rhs$equations

  internal <- sprintf("var %s = this.%s;", dat$meta$internal, dat$meta$internal)
  unpack <- lapply(variables, js_unpack_variable, dat, dat$meta$state, rewrite)

  body <- js_flatten_eqs(c(internal, unpack, eqs[equations]))

  args <- c(dat$meta$time, dat$meta$state, dat$meta$result,
            if (dat$features$has_delay) "solution")
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

  ## TODO: also depend on the string 'random' in generate_js_sexp
  args <- c(dat$meta$time, dat$meta$state, dat$meta$result, "random")
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

  args <- c(dat$meta$time, dat$meta$state,
            if (dat$features$has_delay) "solution")
  js_function(args, body)
}


generate_js_core_names <- function() {
  js_function(c(), "return this.metadata.ynames.slice(1);")
}


generate_js_core_get_metadata <- function() {
  js_function(c(), "return this.metadata;")
}


generate_js_core_get_internal <- function() {
  js_function(c(), "return this.internal;")
}


generate_js_core_update_metadata <- function(eqs, dat, rewrite) {
  body <- collector()
  body$add("this.metadata = {};")
  body$add("var internal = this.internal;")
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
    body$add(ynames)
  } else {
    ynames <- c(dat$meta$time,
                names(dat$data$variable$contents),
                names(dat$data$output$contents))
    body$add("this.metadata.ynames = [%s];",
             paste(dquote(ynames), collapse = ", "))
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
      len <- js_dict(vcapply(contents, generate_js_dim, rewrite))
      sprintf("this.metadata.%sOrder = %s;", location, len)
    }
  }

  body$add(len_block("internal"))
  body$add(len_block("variable"))
  body$add(len_block("output"))

  if (dat$features$has_interpolate) {
    args_min <- vcapply(dat$interpolate$min, function(x)
      sprintf("%s[0]", rewrite(x)))
    args_max <- vcapply(dat$interpolate$max, function(x)
      sprintf("%s[%s - 1]", rewrite(x),
              rewrite(dat$data$elements[[x]]$dimnames$length)))
    array <- function(x) {
      sprintf("[%s]", paste(x, collapse = ", "))
    }
    body$add(
      "this.metadata.interpolateTimes = this.base.interpolate.times(%s, %s);",
      array(args_min), array(args_max))
  }

  js_function(NULL, body$get())
}


## This differs to the metadata stored in the continuous time models,
## following what we store in dust, which is a bit more flexible. Yet
## another rough edge to sort out...
generate_js_core_info <- function(eqs, dat, rewrite) {
  body <- collector()
  body$add("const ret = [];")
  body$add("const %s = this.%s;", dat$meta$internal, dat$meta$internal)

  for (el in dat$data$elements[names(dat$data$variable$contents)]) {
    if (el$rank == 0) {
      dim <- ""
      len <- 1
    } else if (el$rank == 1) {
      len <- rewrite(el$dimnames$length)
      dim <- len
    } else {
      len <- rewrite(el$dimnames$length)
      dim <- paste(vcapply(el$dimnames$dim[[i]], rewrite), collapse = ", ")
    }
    body$add('ret.push({ dim: [%s], length: %s, name: "%s"});',
             dim, len, el$name)
  }

  body$add("return ret;")

  js_function(c(), body$get())
}


generate_js_core_size <- function(eqs, dat, rewrite) {
  body <- c(
    sprintf("const %s = this.%s;", dat$meta$internal, dat$meta$internal),
    sprintf("return %s;", rewrite(dat$data$variable$length)))
  js_function(c(), body)
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
  body$add("var %s = Array(%s).fill(0);",
           dat$meta$state, rewrite(dat$data$variable$length))
  body$add(initial)
  body$add("return %s;", dat$meta$state)

  args <- dat$meta$time
  js_function(args, body$get())
}


generate_js_generator <- function(core, dat) {
  field <- function(name, x) {
    x[[1]] <- sprintf("%s = %s", name, x[[1]])
    x
  }
  method <- function(name, x) {
    x[[1]] <- sub("^function", name, x[[1]])
    x
  }

  is_discrete <- dat$features$discrete

  body <- collector()
  body$add(core$create)
  body$add(method("initial", core$initial_conditions))
  body$add(method("setUser", core$set_user))
  body$add(method("getInternal", core$get_internal))

  if (is_discrete) {
    body$add(method("update", core$update))
    body$add(method("info", core$info))
    body$add(method("size", core$size))
  } else {
    body$add(method("rhs", core$rhs))
    if (!is.null(core$output)) {
      body$add(method("output", core$output))
    }
    body$add(method("names", core$names))
    body$add(method("updateMetadata", core$update_metadata))
    body$add(method("getMetadata", core$get_metadata))
  }

  code <- c(sprintf("class %s {", dat$config$base),
            paste0("  ", body$get()),
            "}")

  code
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
  len
}
