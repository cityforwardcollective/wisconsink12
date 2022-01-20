# Startup message when package is attached.

.tables <- load("data/school_data.RData")
.tables <- .tables[order(.tables)]

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "The following tables are now available:\n",
          paste("- ", .tables, "\n", sep = "")
  )
}

#' List available tables
#'
#' List tables that are made available by the
#' \code{wisconsink12} package.
#'
#' @export list_tables
list_tables <- function() {
  .tables
}
