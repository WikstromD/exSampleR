# Package load hooks
.onLoad <- function(libname, pkgname) {
  invisible(TRUE)
}

.onAttach <- function(libname, pkgname) {
  pkg_env <- as.environment(paste0("package:", pkgname))
  if (!exists("exSample_Distributions", envir = pkg_env, inherits = FALSE)) {
    try(utils::data(list = "exSample_Distributions", package = pkgname, envir = pkg_env), silent = TRUE)
  }
  invisible(TRUE)
}
