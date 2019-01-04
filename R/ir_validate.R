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
  jsonvalidate_version <- utils::packageVersion("jsonvalidate")
  engine <- if (jsonvalidate_version > "1.0.0") "ajv" else "imjv"
  jsonvalidate::json_validate(x, ir_schema(),
                              verbose = TRUE, greedy = TRUE, error = error,
                              engine = "imjv")
}
