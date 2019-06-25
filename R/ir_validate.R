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
  schema <- read_string(path)

  ## We get somewhat better errors from jsonlite's parsers than hoping
  ## that the json is valid.
  jsonlite::fromJSON(schema)

  check_jsonvalidate_version()
  jsonvalidate::json_validator(schema, engine = "ajv")
}


check_jsonvalidate_version <- function(version = NULL) {
  version <- version %||% utils::packageVersion("jsonvalidate")
  if (version < numeric_version("1.1.0")) {
    stop("jsonvalidate >= 1.1.0 is required to validate odin's ir",
         call. = FALSE)
  }
  TRUE
}
