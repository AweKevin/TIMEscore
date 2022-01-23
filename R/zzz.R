##' @importFrom utils packageDescription

.onAttach <- function(libname, pkgname) {
  pkgVersion <- packageDescription(pkgname, fields = "Version")
  msg <- paste0("Welcome to ", pkgname, " v ", pkgVersion, "!")
  citation <- paste0(
    " If you use ", pkgname, " in published research, please acknowledgements:\n",
    "We thank Nan of one-third lab."
  )
  packageStartupMessage(paste0(msg, citation))
}
