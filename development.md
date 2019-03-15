_This might become a vignette at some point_

There are relatively few user-facing functions in this package but rather a lot going on behind the scenes.

## Parse phase

The function `odin_parse` (and `odin_parse_`) convert a model from R's syntax into an intermediate representation that includes all the information to compile the model to another language.  The actual parse phase is carried out with a large number of functions that have the prefix `ir_parse`, in files prefixed by `ir_parse`.  Over time these will be better organised and I'll document what happens in each phase.

## IR serialisation

Even when not persisted we serialise the IR all the way to `json` using [`jsonlite`](https://cran.r-project.org/package=jsonlite); this happens in `ir_serialise.R`.  All functions are prefixed with `ir_serialise`

## IR validation

If requested, then the intermediate representation is validated against the schema (written in jsonSchema); this happens within `ir_validate.R`, using the package [`jsonvalidate`](https://cran.r-project.org/package=jsonvalidate).

## IR deserialisation

Before using the IR we deserialise (mostly trivial, but some lists-of-objects aquire names and character vectors are simplified), which happens in `ir_deserialise.R`.

## Generation

There are two paths of generation - into transpiling R and compiling C.  The files for doing this start at `generate_r.R` and `generate_c.R` respectively.  The end point for the the R and C generation is in `generate_r_class.R` and `generate_c_class` respectively which generates an [`R6`](https://cran.r-project.org/package=R6) class and a constructor function.
