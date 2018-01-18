ODIN_VERSION <- NULL
.onLoad <- function(libname, pkgname) {
  ODIN_VERSION <<- odin_version()
}

odin_version <- function() {
  list(odin = utils::packageVersion("odin"),
       cinterpolate = utils::packageVersion("cinterpolate"),
       r = getRversion(),
       platform = version$platform)
}
