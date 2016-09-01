TEST_VERBOSE <- FALSE

on_appveyor <- function() {
  identical(Sys.getenv("APPVEYOR"), "True")
}
