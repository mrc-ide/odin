## Eventually this should be cached within a session, but wait until
## it works.
ir_schema <- function() {
  path <- system.file("schema.json", package = "odin", mustWork = TRUE)
  dat <- readChar(path, file.size(path))
  ## We get somewhat better errors from jsonlite's parsers than hoping
  ## that the json is valid.
  jsonlite::fromJSON(dat)
  dat
}


ir_validate <- function(x, error = FALSE) {
  ir_validate_against_schema(x, error)
}


odin_validator <- function() {
  jsonvalidate_version <- utils::packageVersion("jsonvalidate")
  engine <- if (jsonvalidate_version > "1.0.0") "ajv" else "imjv"
  jsonvalidate::json_validator(ir_schema(), engine)
}


ir_validate_against_schema <- function(x, error) {
  if (is.null(.odin$validator)) {
    .odin$validator <- odin_validator()
  }
  .odin$validator(x, verbose = TRUE, greedy = TRUE, error = error)
}
