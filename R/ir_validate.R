ir_validate <- function(x, error = FALSE) {
  ir_validate_against_schema(x, error)
}


ir_validate_against_schema <- function(x, error) {
  if (is.null(.odin$validator)) {
    .odin$validator <- ir_validate_create_validator()
  }
  .odin$validator(x, verbose = TRUE, greedy = TRUE, error = error)
}


ir_validate_create_validator <- function() {
  path <- system.file("schema.json", package = "odin", mustWork = TRUE)
  schema <- readChar(path, file.size(path))

  ## We get somewhat better errors from jsonlite's parsers than hoping
  ## that the json is valid.
  jsonlite::fromJSON(schema)

  jsonvalidate_version <- utils::packageVersion("jsonvalidate")
  engine <- if (jsonvalidate_version > "1.0.0") "ajv" else "imjv"
  jsonvalidate::json_validator(schema, engine)
}
