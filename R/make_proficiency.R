#' Get school-level proficiency data
#'
#' Creates a dataframe of school-level proficiency for all tested grades.
#'
#'
#' @import dplyr
#' @import tidyr
#'
#' @export make_proficiency

# Make Proficiency =================================================================

make_proficiency <- function() {

  # public act

  act_test_results <- act |>
    right_join(schools, by = c("school_year", "dpi_true_id")) |>
    filter(group_by_value == "All Students" &
             test_subject %in% c("ELA", "Mathematics") &
             accurate_agency_type != "Private") |>
    mutate(pa = case_when(test_result %in% c("Below Basic", "Basic") ~ "bb",
                          test_result %in% c("Proficient", "Advanced") ~ "pa",
                          test_result == "No Test" ~ "no_test",
                          TRUE ~ "REDACTED")) |>
    filter(pa != "REDACTED")

  # private act
  private  <- schools |>
    filter(accurate_agency_type == "Private") |>
    select(dpi_true_id, school_year, accurate_agency_type) |>
    left_join(forward_exam, by = c("school_year", "dpi_true_id")) |>
    filter(test_group == "ACT") |>
    mutate(pa = case_when(test_result %in% c("Below Basic", "Basic") ~ "bb",
                          test_result %in% c("Proficient", "Advanced") ~ "pa",
                          test_result == "No Test" ~ "no_test",
                          TRUE ~ "REDACTED")) |>
    filter(pa != "REDACTED" & test_subject %in% c("ELA", "Mathematics"))

  # aspire

  aspire <- forward_exam |>
    filter(group_by_value == "All Students" &
             test_group == "Aspire" &
             test_subject %in% c("ELA", "Mathematics")) |>
    right_join(schools, by = c("school_year", "dpi_true_id")) |>
    mutate(pa = case_when(test_result %in% c("Below Basic", "Basic") ~ "bb",
                          test_result %in% c("Proficient", "Advanced") ~ "pa",
                          test_result == "No Test" ~ "no_test",
                          TRUE ~ "REDACTED")) |>
    filter(pa != "REDACTED")

  all <- bind_rows(aspire, private, act_test_results)

  # summarise at school level for hs grades

  hs <- all |>
    group_by(school_year, dpi_true_id, test_subject, pa) |>
    summarise(total_count = sum(student_count, na.rm = TRUE))

  # see main.R for source of `f`
  # contains Forward

  elem <- forward_exam |>
    filter(group_by_value == "All Students" &
             test_group == "Forward" &
             test_subject %in% c("ELA", "Mathematics")) |>
    right_join(schools, by = c("school_year", "dpi_true_id")) |>
    mutate(pa = case_when(test_result %in% c("Below Basic", "Basic") ~ "bb",
                          test_result %in% c("Proficient", "Advanced") ~ "pa",
                          test_result == "No Test" ~ "no_test",
                          TRUE ~ "REDACTED")) |>
    group_by(school_year, dpi_true_id, test_subject, pa) |>
    summarise(total_count = sum(student_count, na.rm = TRUE))


  all_prof <- bind_rows(hs, elem) |>
    group_by(school_year,
             dpi_true_id,
             test_subject,
             pa) |>
    summarise(total_count = sum(total_count)) |>
    ungroup() |>
    group_by(school_year, dpi_true_id, test_subject) |>
    mutate(perc = (total_count / sum(total_count))) |>
    filter(pa == "pa") |>
    # select(-total_count) |>
    ungroup()

  return(all_prof)
}

# prof <- make_proficiency()
