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
#'
#' @importFrom magrittr %>%
#'
#' @export make_mke_schools
#' @export make_mke_rc
#' @export make_wi_rc
#' @export make_mke_enrollment
#' @export est_subgroup_enrollment

# School Lists =================================================================
make_mke_schools <- function() {
  mke_schools <<- schools %>%
    filter((.$MPCP == 1 & county == "Milwaukee") |
            (district_name == "Milwaukee" | (city == "Milwaukee" & accurate_agency_type != "Private School" & locale_description != "Suburb")))
}

# Report Card Lists =================================================================

#' @describeIn make_mke_schools Make a dataframe of Milwaukee schools' Report Card data.
make_mke_rc <- function(private_type = "choice") {
  make_mke_schools()

  if(private_type == "choice") {
    mke_rc <<- report_cards %>%
      filter(dpi_true_id %in% mke_schools$dpi_true_id & (report_card_type != "Private - All Students" | is.na(report_card_type))) %>%
      left_join(., schools %>% select(dpi_true_id, school_name, broad_agency_type, accurate_agency_type), by = "dpi_true_id")

    message("Choosing 'Private - Choice Students' report card type for private schools.")
  } else if(private_type == "all") {
    mke_rc <<- report_cards %>%
     filter(dpi_true_id %in% mke_schools$dpi_true_id & !(has_2_rc == 1 & report_card_type == "Private - Choice Students")) %>%
      left_join(., schools %>% select(dpi_true_id, school_name, broad_agency_type, accurate_agency_type), by = "dpi_true_id")

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

#' @describeIn make_mke_schools Make a dataframe of overall enrollment of Milwaukee resident children.
make_mke_enrollment <- function(agency_type = "broad") {

  if(agency_type == "broad") {
    mke_schools <- schools %>%
      filter(broad_agency_type != "Private" & city == "Milwaukee" & locale_description != "Suburb")

    mke_rc <- report_cards %>%
      filter(dpi_true_id %in% mke_schools$dpi_true_id) %>%
      left_join(., schools %>% select(dpi_true_id, school_name, broad_agency_type), by = "dpi_true_id")

    mke_enrollment_bat <- mke_rc %>%
      select(school_year, broad_agency_type, "total_enrollment" = school_enrollment) %>%
      bind_rows(., other_enrollment %>% select(-accurate_agency_type)) %>%
      group_by(school_year, broad_agency_type) %>%
      summarise(total_enrollment = sum(total_enrollment, na.rm = TRUE))

    mpcp <- choice_counts %>%
      group_by(school_year) %>%
      summarise(MPCP = sum(MPCP_count, na.rm = TRUE))

    mke_snsp <- choice_counts %>%
      filter(MPCP_count > 0) %>%
      group_by(school_year) %>%
      summarise(SNSP = sum(SNSP_count, na.rm = TRUE))

    mpcp_snsp <- left_join(mpcp, mke_snsp, by = "school_year") %>%
      mutate(total_enrollment = MPCP + SNSP,
             broad_agency_type = "MPCP/SNSP") %>%
      select(-c(MPCP, SNSP))

    mke_enrollment_bat <<- bind_rows(mke_enrollment_bat, mpcp_snsp)


  } else if(agency_type == "accurate") {
    mke_schools <- schools %>%
      filter(broad_agency_type != "Private" & city == "Milwaukee" & locale_description != "Suburb")

    mke_rc <- report_cards %>%
      filter(dpi_true_id %in% mke_schools$dpi_true_id & (report_card_type != "Private - All Students" | is.na(report_card_type))) %>%
      left_join(., schools %>% select(dpi_true_id, school_name, accurate_agency_type), by = "dpi_true_id")

    mke_enrollment_aat <- mke_rc %>%
      select(school_year, accurate_agency_type, "total_enrollment" = school_enrollment) %>%
      bind_rows(., other_enrollment %>% select(-broad_agency_type)) %>%
      group_by(school_year, accurate_agency_type) %>%
      summarise(total_enrollment = sum(total_enrollment, na.rm = TRUE))

    mpcp <- choice_counts %>%
      group_by(school_year) %>%
      summarise(MPCP = sum(MPCP_count, na.rm = TRUE))

    mke_snsp <- choice_counts %>%
      filter(MPCP_count > 0) %>%
      group_by(school_year) %>%
      summarise(SNSP = sum(SNSP_count, na.rm = TRUE))

    mpcp_snsp <- left_join(mpcp, mke_snsp, by = "school_year") %>%
      pivot_longer(cols = 2:3, names_to = "accurate_agency_type", values_to = "total_enrollment")

    mke_enrollment_aat <<- bind_rows(mke_enrollment_aat, mpcp_snsp)

  } else {
    stop("Did you specify 'broad' or 'accurate' for agency_type?")
  }
}


# Estimate Subgroup Enrollment =================================================================

#' @describeIn est_subgroup_enrollment Estimate Subgroup enrollment at the school-level based on Report Card numbers.
est_subgroup_enrollment <- function(private_type = "choice", mke = TRUE) {

  race <- c("per_am_in",
            "per_asian",
            "per_b_aa",
            "per_hisp_lat",
            "per_nh_opi",
            "per_white",
            "per_tom")

  report_cards %>%
    filter(report_card_type != "Private - All Students" | is.na(report_card_type)) %>%
    select(dpi_true_id, school_year, school_enrollment, starts_with("per")) %>%
    pivot_longer(cols = starts_with("per"), names_to = "group", values_to = "percent") %>%
    mutate(est_enrollment = school_enrollment * percent)


  message("Choosing 'Private - Choice Students' report card type for private schools.")
}






