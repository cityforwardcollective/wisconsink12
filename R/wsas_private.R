library(tidyverse)
library(readxl)

files <- list.files(path = "./imports/wsas/private")

# Set to NULL because using !exists() doesn't work in for loop
choice_forward <- NULL

fix_format <- function(x) {
  x <- select(x, -contains("Required"))
  colnames(x) <- c("school_year",
                   "grade",
                   "enrollment",
                   "parent_opt_out",
                   "not_tested",
                   "Below Basic",
                   "Basic",
                   "Proficient",
                   "Advanced",
                   "dpi_true_id",
                   "school_name")

  x <- x %>%
    mutate_at(6:9, as.integer) %>%
    mutate(group_count = `Below Basic` + Basic + Proficient + Advanced) %>%
    gather(key = "test_result", value = "student_count", `Below Basic`:Advanced)
}

for (file in files) {

  filename <- paste("imports/wsas/private", file, sep = "/")

  if(is.null("choice_forward")) {
    raw_choice <- read_xlsx(filename, "Opt Out Not Included", skip = 1, col_names = TRUE)

    raw_choice <- raw_choice %>% select(-contains("%")) %>%
      mutate(dpi_true_id = str_replace_all(str_extract(raw_choice$`School Name and Number`, "\\(.*\\)$"), "\\(|\\)", ""),
             dpi_true_id = ifelse(is.na(dpi_true_id), dpi_true_id, paste("0000", dpi_true_id, sep = "_")),
             school_name = ifelse(!str_detect(raw_choice$`School Name and Number`, "-|\\("), raw_choice$`School Name and Number`,
                                  str_replace(str_extract(raw_choice$`School Name and Number`, ".*-|.*\\("), "-|\\(", "")))

    choice_ela <- raw_choice[, c(1, 6:14, 39:40)]
    choice_math <- raw_choice[, c(1, 6, 15:22, 39:40)]
    choice_ss <- raw_choice[, c(1, 6, 23:30, 39:40)]
    choice_science <- raw_choice[, c(1, 6, 31:40)]

    choice_subjects <- list(ELA = choice_ela, Mathematics = choice_math, Science = choice_science, "Social Studies" = choice_ss)

    choice_gathered <- map(choice_subjects, fix_format)

    choice_gathered <- bind_rows(choice_gathered, .id = "test_subject")
    choice_forward <- choice_gathered %>%
      mutate(test_group = ifelse(grade %in% 3:8, "Forward",
                                 ifelse(grade == 10 & test_subject == "social studies", "Forward",
                                        ifelse(grade %in% 9:10, "Aspire",
                                               ifelse(grade == 11, "ACT",
                                                      "ERROR")))),
             opt_outs_excluded = enrollment - parent_opt_out,
             group_by = "All Students",
             group_by_value = "All Students") %>%
      modify_at("student_count", as.integer) %>%
      filter(grade != "Total" & test_group == "Forward" & !str_detect(school_name, "Choice Program")) %>%
      select(-c(school_name, enrollment))
  }

  else {raw_choice1 <- read_xlsx(filename, "Opt Out Not Included", skip = 1, col_names = TRUE)

  raw_choice1 <- raw_choice1 %>% select(-contains("%")) %>%
    mutate(dpi_true_id = str_replace_all(str_extract(raw_choice1$`School Name and Number`, "\\(.*\\)$"), "\\(|\\)", ""),
           dpi_true_id = ifelse(is.na(dpi_true_id), dpi_true_id, paste("0000", dpi_true_id, sep = "_")),
           school_name = ifelse(!str_detect(raw_choice1$`School Name and Number`, "-|\\("), raw_choice1$`School Name and Number`,
                                str_replace(str_extract(raw_choice1$`School Name and Number`, ".*-|.*\\("), "-|\\(", "")))

  choice1_ela <- raw_choice1[, c(1, 6:14, 39:40)]
  choice1_math <- raw_choice1[, c(1, 6, 15:22, 39:40)]
  choice1_ss <- raw_choice1[, c(1, 6, 23:30, 39:40)]
  choice1_science <- raw_choice1[, c(1, 6, 31:40)]

  choice1_subjects <- list(ELA = choice1_ela, Mathematics = choice1_math, Science = choice1_science, "Social Studies" = choice1_ss)

  choice1_gathered <- map(choice1_subjects, fix_format)

  choice1_gathered <- bind_rows(choice1_gathered, .id = "test_subject")
  choice1_forward <- choice1_gathered %>%
    mutate(test_group = ifelse(grade %in% 3:8, "Forward",
                               ifelse(grade == 10 & test_subject == "social studies", "Forward",
                                      ifelse(grade %in% 9:10, "Aspire",
                                             ifelse(grade == 11, "ACT",
                                                    "ERROR")))),
           opt_outs_excluded = enrollment - parent_opt_out,
           group_by = "All Students",
           group_by_value = "All Students") %>%
    modify_at("student_count", as.integer) %>%
    modify_at("group_count", as.integer) %>%
    filter(grade != "Total" & test_group == "Forward" & !str_detect(school_name, "Choice Program")) %>%
    select(-c(school_name, enrollment))
  }


  choice_forward <- bind_rows(choice_forward, choice1_forward)

}
