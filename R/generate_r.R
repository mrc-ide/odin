## Generate an interface.  This is a bit tricky as we want to generate
## very slightly different interfaces based on the options in info.
## For now this is done sub-optimally but I think it's fine for now at
## least.
##
## TODO: Consider an option here to return either the generator or the
## function that tries to collect the variables up and skip $new.
##
## TODO: Do we also want something that will act as "destroy" and
## unload the DLL and void all the pointers?  That requires that we
## keep a pointer cache here, but that's easy enough.  We can register
## this for eventual garbage collection too, so that's nice.
##
## When doing codegen we can build out interfaces that don't include
## much R switching, can get the code bits dealt with correctly.


## OK, this step here is the only part that generates *R* code.
odin_generate_r <- function(info, dll) {
  discrete <- info$discrete
  base <- info$base

  ## TODO: Need to get dll into this, which is a right pain because I
  ## don't know that we know it at this point...
  ##
  ## TODO: decide if we're using DDE at which stage?
  ret <- collector()
  ret$add(".R6_%s <- R6::R6Class(", base)
  ret$add('  "odin_model",')
  ## This is needed to access some internal odin functions; I think
  ## that's OK though?  From odin the main thing we use is
  ## odin_prepare(), which in turn uses some nasty bits in
  ## make_translate.  I need to see if the package will load with odin
  ## not loaded though.
  ret$add("  parent_env = environment(odin::odin),")
  ret$add("  public = list(")
  ret$add('    name = "%s",', base)
  ret$add("    ptr = NULL,")
  if (!discrete) {
    ret$add("    use_dde = NULL,")
  }
  ret$add("    ## Cache:")
  ret$add("    init = NULL,")
  ret$add("    variable_order = NULL,")
  if (info$has_output) {
    ret$add("    output_order = NULL,")
    ret$add("    output_length = NULL,")
  }
  ret$add("    names = NULL,")
  ret$add("    transform_variables = NULL,")
  if (info$has_interpolate) {
    ret$add("    interpolate_t = NULL,")
  }
  ret$add("    ## Methods:")
  methods <- list(
    odin_generate_r_initialize(info, dll),
    if (info$has_user) odin_generate_r_set_user(info, dll),
    odin_generate_r_update_cache(info, dll),
    if (!discrete && !info$has_delay) odin_generate_r_deriv(info, dll),
    if ( discrete && !info$has_delay) odin_generate_r_update(info, dll),
    odin_generate_r_initial(info, dll),
    odin_generate_r_run(info, dll),
    odin_generate_r_contents(info, dll))
  methods <- methods[!vlapply(methods, is.null)]
  methods <-
    paste(vcapply(methods, function(x) paste(indent(x, 4), collapse="\n")),
          collapse=",\n\n")
  ret$add(methods)
  ret$add("  ))")
  ret$add(odin_generate_r_function(info, dll))
  ret$get()
}

odin_generate_r_function <- function(info, dll) {
  ret <- collector()
  base <- info$base
  if (info$has_user) {
    ## TODO: this needs a bunch of work with getting the defaults
    ## because they're not available in the info object :(
    ##
    ## To get them here I really need to prevent generation of the
    ## info function first.
    ##
    ## NOTE: user_default is the wrong polarity here; flip that in generation
    user <- info$user_default[order(!info$user_default)]
    collector <- sprintf("list(%s)",
                         pastec(sprintf("%s = %s", names(user), names(user))))
    args <- c(vcapply(info$user_default[order(!info$user_default)],
                     function(x) if (x) "" else "NULL"),
              c(user = collector))
    args_use <- "user = user"
  } else {
    args <- args_use <- character(0)
  }
  if (!info$discrete) {
    args <- c(args, c(use_dde = "FALSE"))
    args_use <- c(args_use, "use_dde = use_dde")
  }

  ## TODO: This will need to go through a parse/deparse step to get
  ## nicely formatted probably?  Not sure.  Not really worth stressing
  ## about for now though.
  i <- nzchar(args)
  args_list <- names(args)
  args_list[i] <- sprintf("%s = %s", args_list[i], args[i])
  ret$add("%s <- function(%s) {", base, pastec(args_list))
  ret$add("  .R6_%s$new(%s)", base, pastec(args_use))
  ret$add("}")
  ret$get()
}

odin_generate_r_update_cache <- function(info, dll) {
  ret <- collector()
  base <- info$base
  ret$add("update_cache = function() {")
  ret$add("  self$variable_order <- %s",
          dot_call(base, dll, "%s_variable_order", "self$ptr"))
  ## TODO: don't generate output_order if no output
  if (info$has_output) {
    ret$add("  self$output_order <- %s",
            dot_call(base, dll, "%s_output_order", "self$ptr"))
  }
  if (info$has_interpolate) {
    ret$add("      self$interpolate_t <- %s",
            dot_call(base, dll, "%s_interpolate_t", "self$ptr"))
  }
  ## The mechanics by which this happen I don't know; at the moment
  ## this is OK via the parent env flag but this could cause trouble
  ## with R CMD check in generated packages which I would rather
  ## avoid.
  ret$add("  odin_prepare(self, %s)", info$discrete)
  ret$add("}")
  ret$get()
}

odin_generate_r_initialize <- function(info, dll) {
  ret <- collector()
  base <- info$base
  if (info$discrete) {
    ret <- collector()
    ret$add("initialize = function(user=NULL) {")
    ret$add("  self$ptr <- %s", dot_call(base, dll, "%s_create", "user"))
  } else {
    ret$add("initialize = function(user=NULL, use_dde=FALSE) {")
    ret$add("  self$use_dde <- use_dde")
    ret$add("  self$ptr <- %s",
            dot_call(base, dll, "%s_create", "user", "use_dde"))
  }
  if (info$initial_stage < STAGE_TIME) {
    ret$add("  self$init <- %s",
            dot_call(base, dll, "%s_initialise", "self$ptr", "NA_real_"))
  }
  ## Until I cave and make dde/ring a proper dependency, try and be
  ## well behaved here.
  if (info$discrete) {
    ret$add('  loadNamespace("dde")')
  } else {
    ret$add("  if (use_dde) {")
    ret$add('    loadNamespace("dde")')
    ret$add("  }")
  }
  ret$add("  self$update_cache()")
  ret$add("}")
  ret$get()
}

odin_generate_r_set_user <- function(info, dll) {
  ret <- collector()
  base <- info$base
  ## TODO: generate full interface here with the bits above, once
  ## they're written?
  ret$add("set_user = function(..., user = list(...)) {")
  ret$add("  %s", dot_call(base, dll, "r_%s_set_user", "self$ptr", "user"))
  if (info$initial_stage == STAGE_USER) {
      ret$add("  self$init <- %s",
              dot_call(base, dll, "%s_initialise", "self$ptr", "NA_real_"))
  }
  if (info$dim_stage == STAGE_USER) {
    ret$add("  self$update_cache()")
  }
  ret$add("  invisible(self$init)")
  ret$add("}")
  ret$get()
}

odin_generate_r_deriv <- function(info, dll) {
  ret <- collector()
  ret$add("deriv = function(%s, y) {", TIME)
  ret$add("  %s", dot_call(info$base, dll,
                           "%s_deriv_r", "self$ptr", TIME, "y"))
  ret$add("}")
  ret$get()
}

odin_generate_r_initial <- function(info, dll) {
  ret <- collector()
  time <- if (info$discrete) STEP else TIME
  ret$add("initial = function(%s) {", time)
  if (info$initial_stage < STAGE_TIME) {
    ret$add("  self$init")
  } else {
    time_use <-
      sprintf("as.%s(%s)", if (info$discrete) "integer" else "numeric", time)
    ## TODO: Consider a better name here?
    ret$add("  %s",
            dot_call(info$base, dll, "%s_initialise", "self$ptr", time_use))
  }
  ret$add("}")
}

odin_generate_r_update <- function(info, dll) {
  ## TODO: also rewrite STATE?  Here and everywhere where 'y' occurs?
  ret <- collector()
  ret$add("update = function(%s, y) {", STEP)
  ret$add("  %s", dot_call(info$base, dll,
                           "%s_update_r", "self$ptr", STEP, "y"))
  ret$add("}")
  ret$get()
}

odin_generate_r_run <- function(info, dll) {
  ret <- collector()
  discrete <- info$discrete
  base <- info$base
  has_interpolate <- info$has_interpolate
  has_delay <- info$has_delay
  time <- if (discrete) STEP else TIME

  if (discrete) {
    ret$add("run = function(%s, y = NULL, ..., use_names = TRUE) {", STEP)
  } else if (has_interpolate) {
    ret$add(
      "run = function(%s, y = NULL, ..., tcrit = NULL, use_names = TRUE) {",
      time)
  } else {
    ret$add(
      "run = function(%s, y = NULL, ..., use_names = TRUE) {",
      time)
  }
  ret$add("  if (is.null(y)) {")
  if (info$initial_stage < STAGE_TIME) {
    ret$add("    y <- self$init", time)
  } else {
    ret$add("    y <- self$initial(%s[[1L]])", time)
  }
  if (info$has_delay) {
    ret$add("  } else {")
    ret$add("    %s",
            dot_call(base, dll, "%s_set_initial", "self$ptr", time, "y"))
  }
  ret$add("  }")
  if (info$has_interpolate) {
    ret$add(indent(odin_generate_r_run_interpolate_check(info), 2))
  }
  if (discrete) {
    ret$add('  ret <- dde::difeq(y, %s, "%s_update_dde", self$ptr,',
            time, base)
    ret$add(indent(sprintf('dllname="%s",', dll), 20))
    if (info$has_output) {
      ret$add(indent(sprintf('n_out=self$output_length,', base), 20))
    }
    if (info$has_delay) {
      ## TODO: make this a default like tcrit is; that's *much* easier
      ## now that we're doing codegen.  Wait until I factor this out
      ## into its own function though.
      ret$add(indent("n_history=1000L, return_history=FALSE,", 20))
    }
    ret$add(indent("parms_are_real=FALSE,", 20))
    ## Try and preserve some compatibility with deSolve:
    ret$add(indent("deSolve_compatible=TRUE, ...)", 20))
  } else {
    ## OK throughout here I think I'll break this up a little and do
    ## it as argument collection / formatting.
    ret$add("  if (self$use_dde) {")
    ret$add('    ret <- dde::dopri(y, %s, "%s_deriv_dde", self$ptr,',
            time, base)
    ret$add(indent(sprintf('dllname="%s",', dll), 22))
    if (info$has_output) {
      ret$add(indent(sprintf(
        'n_out=self$output_length, output="%s_output_dde",',
        base), 22))
    }
    if (info$has_delay) {
      ## TODO: make this a default like tcrit is; that's *much* easier
      ## now that we're doing codegen.  Wait until I factor this out
      ## into its own function though.
      ret$add(indent("n_history=1000L, return_history=FALSE,", 22))
    }
    ret$add(indent("parms_are_real=FALSE,", 22))
    ## Try and preserve some compatibility with deSolve:
    ret$add(indent("by_column=TRUE, return_initial=TRUE,", 22))
    ret$add(indent("return_time=TRUE, return_output_with_y=TRUE, ...)", 22))
    ret$add("  } else {")
    len <- if (info$has_delay) 25 else 24
    ret$add('    ret <- deSolve::%s(y, %s, "%s_deriv_ds", self$ptr,',
            if (info$has_delay) "dede" else "ode", time, base)
    ret$add(indent(sprintf('initfunc = "%s_initmod_ds", dllname = "%s",',
                           base, dll), len))
    if (info$has_output) {
      ret$add(indent("nout = self$output_length,", len))
    }
    if (info$has_interpolate) {
      ret$add(indent("tcrit = tcrit,", len))
    }
    ret$add(indent("...)", len))
    ret$add("  }")
  }
  ret$add("  if (use_names) {")
  ret$add("    colnames(ret) <- self$names")
  ret$add("  } else {")
  ret$add("    colnames(ret) <- NULL")
  ret$add("  }")
  ret$add("  ret")
  ret$add("}")
  ret$get()
}

## NOTE: factored out mostly because of really terrible line length issues
##
## TODO: it's possible we could haul the code in directly and deparse?
##
## TODO: Some care will be needed when dealing with discrete
## models here
odin_generate_r_run_interpolate_check <- function(info) {
  time_name <- if (info$discrete) STEP else TIME
  ret <- collector()
  ret$add("r <- self$interpolate_t")
  ret$add("if (%s[[1L]] < r[[1L]]) {", time_name)
  ret$add('  stop("Integration times do not span interpolation range; min: ",')
  ret$add("        r[[1L]])")
  ret$add("}")
  ## TODO: we can look at the max order of the interpolation and
  ## decide whether this clause needs to be added.
  ret$add("if (!is.na(r[[2L]]) && %s[[length(%s)]] > r[[2L]]) {",
          time_name, time_name)
  ret$add('  stop("Integration times do not span interpolation range; max: ",')
  ret$add("        r[[2L]])")
  ret$add("}")
  if (!info$discrete) {
    ret$add("if (is.null(tcrit) && !is.na(r[[2L]]) && !self$use_dde) {")
    ret$add("  tcrit <- r[[2L]]")
    ret$add("}")
  }
  ret$get()
}

odin_generate_r_contents <- function(info, dll) {
  ret <- collector()
  ret$add("contents = function() {")
  ret$add('  %s', dot_call(info$base, dll, "%s_contents", "self$ptr"))
  ret$add("}")
  ret$get()
}

dot_call <- function(base, dll, fmt, ...) {
  args <- paste(..., sep=", ")
  stopifnot(length(args) > 0 && nzchar(args))
  sprintf('.Call("%s", %s, PACKAGE="%s")',
          sprintf(fmt, base), args, dll)
}
