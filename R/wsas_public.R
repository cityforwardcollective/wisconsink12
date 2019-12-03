library(tidyverse)

files <- list.files(path = "./imports/wsas/public")

# Set to NULL because using !exists() doesn't work in for loop
public_forward <- NULL

for(file in files) {

  filename <- paste("imports/wsas/public", file, sep = "/")

  if(is.null("public_forward")) {
    raw_public <- read_csv(filename, col_types = list("c",
                                                      "f",
                                                      "i",
                                                      "c",
                                                      "i",
                                                      "i",
                                                      "f",
                                                      "l",
                                                      "c",
                                                      "c",
                                                      "f",
                                                      "f",
                                                      "f",
                                                      "i",
                                                      "f",
                                                      "f",
                                                      "f",
                                                      "i",
                                                      "c",
                                                      "i",
                                                      "c"))

    colnames(raw_public) <- str_to_lower(colnames(raw_public))

    public_forward <- raw_public %>%
      filter(!str_detect(school_name, pattern = "\\[")) %>%
      mutate(district_code = str_pad(district_code, 4, side = "left", pad = 0),
             school_code = str_pad(school_code, 4, side = "left", pad = 0),
             dpi_true_id = paste(district_code, school_code, sep = "_")) %>%
      select(school_year,
             dpi_true_id,
             test_subject,
             "grade" = grade_level,
             test_result,
             test_group,
             group_by,
             group_by_value,
             student_count,
             group_count)
  }

  else {raw_public1 <- read_csv(filename, col_types = list("c",
                                                           "f",
                                                           "i",
                                                           "c",
                                                           "i",
                                                           "i",
                                                           "f",
                                                           "l",
                                                           "c",
                                                           "c",
                                                           "f",
                                                           "f",
                                                           "f",
                                                           "i",
                                                           "f",
                                                           "f",
                                                           "f",
                                                           "i",
                                                           "c",
                                                           "i",
                                                           "c"))

  colnames(raw_public1) <- str_to_lower(colnames(raw_public1))

  public1_forward <- raw_public1 %>%
    filter(!str_detect(school_name, pattern = "\\[")) %>%
    mutate(district_code = str_pad(district_code, 4, side = "left", pad = 0),
           school_code = str_pad(school_code, 4, side = "left", pad = 0),
           dpi_true_id = paste(district_code, school_code, sep = "_")) %>%
    select(school_year,
           dpi_true_id,
           test_subject,
           "grade" = grade_level,
           test_result,
           test_group,
           group_by,
           group_by_value,
           student_count,
           group_count)
  }


  public_forward <- bind_rows(public_forward, public1_forward)
}
