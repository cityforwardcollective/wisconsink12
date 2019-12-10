#' Startup message when package is attached.

.x <- load("data/school_data.RData")

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "The following tables are now available:\n",
          paste("- ", .x, "\n", sep = "")
  )
}
