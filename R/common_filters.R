#' Make common dataframes
#'
#' Creates commonly-needed dataframes of \code{wisconsink12} data,
#' such as city of Milwaukee schools and their report cards.
#'
#' @param private_type Select which type of Report Card to be included for choice schools.
#' Options are 'choice' for the 'Private - Choice Students' and 'all' for 'Private - All Students'
#' Report Card types.
#' @param exclude_milwaukee Logical. If TRUE (default value), Milwaukee schools will be
#' excluded from \code{wi_rc}.
#' @import dplyr
#' @importFrom magrittr %>%
#' @export make_mke_schools
#' @export make_mke_rc
#' @export make_wi_rc

# School Lists =================================================================
make_mke_schools <- function() {
  mke_schools <<- schools %>%
    filter(city == "Milwaukee" & (district_name == "Milwaukee" | accurate_agency_type != "Private School") & locale_description != "Suburb")
}

# Report Card Lists =================================================================

#' @describeIn make_mke_schools Make a dataframe of Milwaukee schools' Report Card data.
make_mke_rc <- function(private_type = "choice") {
  make_mke_schools()

  if(private_type == "choice") {
    mke_rc <<- report_cards %>%
      filter(dpi_true_id %in% mke_schools$dpi_true_id & report_card_type != "Private - All Students")

    message("Choosing 'Private - Choice Students' report card type for private schools.")
  } else if(private_type == "all") {
    mke_rc <<- report_cards %>%
     filter(dpi_true_id %in% mke_schools$dpi_true_id & !(has_2_rc == 1 & report_card_type == "Private - Choice Students"))

    message("Choosing 'Private - All Students' report card type where available for private schools.")
  } else {
    stop("Did you specify 'choice' or 'all' for private_type?")
  }
}

#' @describeIn make_mke_schools Make a dataframe of Wisconsin schools' Report Card data.
make_wi_rc <- function(exclude_milwaukee = TRUE, private_type = "choice") {
  make_mke_schools()

  if(exclude_milwaukee == TRUE) {
    wi_rc <<- report_cards %>%
        filter(!dpi_true_id %in% mke_schools$dpi_true_id)

    message("Excluding Milwaukee schools.")
  } else {
    wi_rc <<- report_cards

    message("Including Milwaukee schools.")
  }

  if(private_type == "choice") {
    wi_rc <<- wi_rc %>%
      filter(report_card_type == "Private - Choice Students")

    message("Choosing 'Private - Choice Students' report card type for private schools.")
  } else if(private_type == "all") {
    wi_rc <<- wi_rc %>%
    filter(report_card_type == "Private - All Students")

    message("Choosing 'Private - All Students' report card type for private schools.")
  } else {
    stop("Did you specify 'choice' or 'all' for private_type?")
  }
}

# Enrollment Lists =================================================================

make_mke_enrollment <- function() {

}
