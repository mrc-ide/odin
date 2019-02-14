generate_check_features <- function(supported, dat) {
  features_used <- vlapply(dat$features, identity)
  msg <- setdiff(names_if(features_used), supported)
  if (length(msg) > 0L) {
    stop("Features not suppored: ", paste(dquote(msg), collapse = ", "))
  }
}
