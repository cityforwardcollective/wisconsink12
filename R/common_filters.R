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
#' @import dplyr
#' @import tidyr
#'
#' @export make_mke_schools
#' @export make_mke_rc
#' @export make_wi_rc
#' @export make_mke_enrollment
#' @export est_subgroup_enrollment

# School Lists =================================================================
make_mke_schools <- function(school_years = "all") {

  # Initialize vector of potential school years
  school_years_ <- unique(schools$school_year)

  # Make sure all school_years values are valid

  if (sum(school_years %in% c(school_years_, "all")) != length(school_years)) {
    stop("Please enter a vector of valid school years.")
  }

  # Filter schools for relevant school years

  if (length(school_years) > 1) {
    mke_schools_ <- schools %>%
      filter(milwaukee_indicator == 1 & school_year %in% school_years)
  } else if (school_years == "all") {
    mke_schools_ <- schools %>%
      filter(milwaukee_indicator == 1)
  } else {
    mke_schools_ <- schools %>%
      filter(milwaukee_indicator == 1 & school_year == school_years)
  }

  return(mke_schools_)

}

# Report Card Lists =================================================================

#' @describeIn make_mke_schools Make a dataframe of Milwaukee schools' Report Card data.
make_mke_rc <- function(private_type = "choice") {

  mke_schools <- make_mke_schools()

  if (private_type == "choice") {
    mke_rc <- report_cards %>%
      filter(report_card_type != "Private - All Students" | is.na(report_card_type)) %>%
      right_join(., mke_schools %>% select(dpi_true_id, school_name, broad_agency_type, accurate_agency_type, school_year)) %>%
      select(school_year,
             dpi_true_id,
             school_name,
             accurate_agency_type,
             broad_agency_type,
             everything())

    return(mke_rc)

    message("Choosing 'Private - Choice Students' report card type for private schools.")

  } else if (private_type == "all") {

    mke_rc <- report_cards %>%
     filter(!(has_2_rc == 1 & report_card_type == "Private - Choice Students")) %>%
      right_join(., mke_schools %>% select(dpi_true_id, school_name, broad_agency_type, accurate_agency_type, school_year)) %>%
      select(school_year,
             dpi_true_id,
             school_name,
             accurate_agency_type,
             broad_agency_type,
             everything())

    return(mke_rc)

    message("Choosing 'Private - All Students' report card type where available for private schools.")

  } else {

    stop("Did you specify 'choice' or 'all' for private_type?")

  }
}

#' @describeIn make_mke_schools Make a dataframe of Wisconsin schools' Report Card data.
make_wi_rc <- function(exclude_milwaukee = TRUE, private_type = "choice") {

  mke_schools <- make_mke_schools()

  if (exclude_milwaukee == TRUE) {

    wi_rc <- report_cards %>%
      right_join(., schools %>%
                   filter(!(accurate_agency_type == "Private" & choice_indicator == 1)) |>
                   select(dpi_true_id, school_name, broad_agency_type, accurate_agency_type, school_year)) %>%
      anti_join(., mke_schools %>% select(dpi_true_id, school_year)) %>%
      select(school_year,
             dpi_true_id,
             school_name,
             accurate_agency_type,
             broad_agency_type,
             everything())

    message("Excluding Milwaukee schools.")

  } else {

    wi_rc <- report_cards %>%
      right_join(., schools %>%
                   filter(!(accurate_agency_type == "Private" & choice_indicator == 1)) |>
                   select(dpi_true_id, school_name, broad_agency_type, accurate_agency_type, school_year)) %>%
      select(school_year,
             dpi_true_id,
             school_name,
             accurate_agency_type,
             broad_agency_type,
             everything())

    message("Including Milwaukee schools.")

  }

  if (private_type == "choice") {

    wi_rc <- wi_rc %>%
      filter(report_card_type != "Private - All Students" | is.na(report_card_type))

    return(wi_rc)

    message("Choosing 'Private - Choice Students' report card type for private schools.")

  } else if (private_type == "all")
    {

    wi_rc <- wi_rc %>%
      filter(!(has_2_rc == 1 & report_card_type == "Private - Choice Students"))

    return(wi_rc)

    message("Choosing 'Private - All Students' report card type for private schools.")

  } else {

    stop("Did you specify 'choice' or 'all' for private_type?")

  }
}

# Enrollment Lists =================================================================

#' @describeIn make_mke_schools Make a dataframe of overall enrollment of Milwaukee resident children.
make_mke_enrollment <- function(agency_type = "broad") {

  if (agency_type == "broad") {

    mke_schools <- make_mke_schools()

    mke_schools <- mke_schools %>%
      filter(broad_agency_type != "Private")

    mke_rc <- enrollment %>%
      filter(group_by_value == "All Students") %>%
      right_join(., mke_schools %>% select(dpi_true_id, school_name, broad_agency_type, school_year))

    mke_enrollment_bat <- mke_rc %>%
      select(school_year, broad_agency_type, "total_enrollment" = student_count) %>%
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

    mke_enrollment_bat <- bind_rows(mke_enrollment_bat, mpcp_snsp)

    return(mke_enrollment_bat)


  } else if (agency_type == "accurate") {

    mke_schools <- make_mke_schools()

    mke_schools <- mke_schools %>%
      filter(broad_agency_type != "Private")

    mke_rc <- enrollment %>%
      filter(group_by_value == "All Students") %>%
      right_join(., mke_schools %>% select(dpi_true_id, school_name, accurate_agency_type, school_year))

    mke_enrollment_aat <- mke_rc %>%
      select(school_year, accurate_agency_type, "total_enrollment" = student_count) %>%
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

    mke_enrollment_aat <- bind_rows(mke_enrollment_aat, mpcp_snsp)

    return(mke_enrollment_aat)

  } else {

    stop("Did you specify 'broad' or 'accurate' for agency_type?")

  }
}


# Estimate Subgroup Enrollment =================================================================

#' @describeIn est_subgroup_enrollment Estimate subgroup enrollment at the school-level based on Report Card numbers.
est_subgroup_enrollment <- function(private_type = "choice") {

  race <- c("count_am_in",
            "count_asian",
            "count_b_aa",
            "count_hisp_lat",
            "count_nh_opi",
            "count_white",
            "count_tom")

  e <- enrollment %>%
    filter(!group_by %in% c("All Students",
                            "Gender",
                            "GENDER_ALT",
                            "Grade Level",
                            "GRADE_ALT",
                            "Migrant Status",
                            "Primary Disability")) %>%
    mutate(group_by = case_when(group_by == "Economic Status" ~ "economic_status",
                                group_by == "ELL Status" ~ "english_proficiency",
                                group_by == "Race/Ethnicity" ~ "race_ethnicity",
                                group_by == "RACE_ALT" ~ "race_ethnicity",
                                group_by == "Disability Status" ~ "students_w_disabilities"),
           group_by_value = case_when(group_by_value == "SwD" ~ "count_swd",
                                      group_by_value == "Econ Disadv" ~ "count_ed",
                                      group_by_value == "ELL/LEP" ~ "count_lep",
                                      group_by_value == "Asian" ~ "count_asian",
                                      group_by_value == "Black" ~ "count_b_aa",
                                      group_by_value == "Hispanic" ~ "count_hisp_lat",
                                      group_by_value == "Two or More" ~ "count_tom",
                                      group_by_value == "White" ~ "count_white",
                                      group_by_value == "Amer Indian" ~ "count_am_in",
                                      group_by_value == "Pacific Isle" ~ "count_nh_opi",
                                      group_by_value == "Unknown" ~ "count_unknown",
                                      str_detect(group_by_value, "Suppressed") ~ "suppressed",
                                      TRUE ~ "Drop")) %>%
    # Filter out the nots (not ED, not SwD, etc)
    filter(group_by_value != "Drop")

  suppressed <- e %>%
    filter(group_by_value == "suppressed") %>%
    select(1:3)

  pub <- e %>%
    filter(group_by_value != "suppressed") %>%
    left_join(., schools %>% select(dpi_true_id, school_year, accurate_agency_type, broad_agency_type),
              by = c("dpi_true_id", "school_year")) %>%
    mutate(is_estimate = 0)

  pub_ed <- report_cards %>%
    left_join(., schools %>% select(school_year, dpi_true_id, accurate_agency_type)) %>%
    filter(accurate_agency_type != "Private") %>%
    select(school_year, dpi_true_id, school_enrollment, per_ed) %>%
    pivot_longer(cols = starts_with("per"), names_to = "group_by_value", values_to = "percent") %>%
    mutate(est_enrollment = school_enrollment * percent,
           group_by_value = str_replace_all(group_by_value, "per", "count"),
           group_by = case_when(group_by_value %in% race ~ "race_ethnicity",
                                group_by_value == "count_ed" ~ "economic_status",
                                group_by_value == "count_lep" ~ "english_proficiency",
                                group_by_value == "count_choice" ~ "choice",
                                group_by_value == "count_open" ~ "open_enrollment",
                                group_by_value == "count_swd" ~ "students_w_disabilities")) %>%
    semi_join(., suppressed) %>%
    select(school_year,
           dpi_true_id,
           group_by_value,
           est_enrollment) %>%
    left_join(., schools %>% select(dpi_true_id, school_year, accurate_agency_type, broad_agency_type),
              by = c("dpi_true_id", "school_year")) %>%
    mutate(group_by = "economic_status",
           is_estimate = 1) %>%
    select(school_year,
           dpi_true_id,
           accurate_agency_type,
           broad_agency_type,
           group_by,
           group_by_value,
           student_count = est_enrollment,
           is_estimate)

  p <- bind_rows(pub, pub_ed)

  if (private_type == "choice") {

    private <- report_cards %>%
      left_join(., schools %>% select(school_year, dpi_true_id, accurate_agency_type)) %>%
      filter((report_card_type == "Private - Choice Students" | is.na(report_card_type)) & accurate_agency_type == "Private") %>%
      select(dpi_true_id, school_year, school_enrollment, starts_with("per")) %>%
      pivot_longer(cols = starts_with("per"), names_to = "group_by_value", values_to = "percent") %>%
      mutate(est_enrollment = school_enrollment * percent,
             group_by_value = str_replace_all(group_by_value, "per", "count")) %>%
      select(dpi_true_id,
             school_year,
             group_by_value,
             est_enrollment) %>%
      left_join(., schools %>% select(dpi_true_id, school_year, accurate_agency_type, broad_agency_type),
                by = c("dpi_true_id", "school_year")) %>%
      mutate(group_by = case_when(group_by_value %in% race ~ "race_ethnicity",
                                  group_by_value == "count_ed" ~ "economic_status",
                                  group_by_value == "count_lep" ~ "english_proficiency",
                                  group_by_value == "count_choice" ~ "choice",
                                  group_by_value == "count_open" ~ "open_enrollment",
                                  group_by_value == "count_swd" ~ "students_w_disabilities"),
             is_estimate = 1) %>%
      select(school_year,
             dpi_true_id,
             accurate_agency_type,
             broad_agency_type,
             group_by,
             group_by_value,
             student_count = est_enrollment,
             is_estimate)



    subgroup_enr <- bind_rows(private, p)

    return(subgroup_enr)

    message("Choosing 'Private - Choice Students' report card type for private schools.")

  } else if (private_type == "all") {

    private <- report_cards %>%
      left_join(., schools %>% select(school_year, dpi_true_id, accurate_agency_type)) %>%
      filter(!(has_2_rc == 1 & report_card_type == "Private - Choice Students") & accurate_agency_type == "Private") %>%
      select(dpi_true_id, school_year, school_enrollment, starts_with("per")) %>%
      pivot_longer(cols = starts_with("per"), names_to = "group_by_value", values_to = "percent") %>%
      mutate(est_enrollment = school_enrollment * percent,
             group_by_value = str_replace_all(group_by_value, "per", "count")) %>%
      select(dpi_true_id,
             school_year,
             group_by_value,
             est_enrollment) %>%
      left_join(., schools %>% select(dpi_true_id, school_year, accurate_agency_type, broad_agency_type),
                by = c("dpi_true_id", "school_year")) %>%
      mutate(group_by = case_when(group_by_value %in% race ~ "race_ethnicity",
                                  group_by_value == "count_ed" ~ "economic_status",
                                  group_by_value == "count_lep" ~ "english_proficiency",
                                  group_by_value == "count_choice" ~ "choice",
                                  group_by_value == "count_open" ~ "open_enrollment",
                                  group_by_value == "count_swd" ~ "students_w_disabilities"),
             is_estimate = 1) %>%
      select(school_year,
             dpi_true_id,
             accurate_agency_type,
             broad_agency_type,
             group_by,
             group_by_value,
             student_count = est_enrollment,
             is_estimate)

    subgroup_enr <- bind_rows(private, p)

    return(subgroup_enr)

    message("Choosing 'Private - All Students' report card type for private schools.")

  } else {

    stop("Did you specify 'choice' or 'all' for private_type?")

  }

}






