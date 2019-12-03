library(tidyverse)

source("./R/enrollment_script.R")

instrumentality <- c("3619_0162",
                     "3619_0413",
                     "3619_0334",
                     "3619_0165",
                     "3619_0398")

unique_schools_ly <- all_enrollment %>%
  modify_at("school_year", factor, ordered = TRUE) %>%
  group_by(dpi_true_id) %>%
  summarise(last_year_open = max(school_year))

unique_schools <- unique_schools_ly %>%
  left_join(., all_enrollment, by = c("dpi_true_id", "last_year_open" = "school_year")) %>%
  mutate(charter_indicator = ifelse(charter_indicator == "Yes", 1, 0),
         choice_indicator = ifelse(choice_identifier == "CHC", 1, 0)) %>%
  select(dpi_true_id, school_name, agency_type, district_name, county, choice_indicator, charter_indicator,
         last_year_open) %>%
  unique() %>%
  mutate(choice_indicator = ifelse(is.na(choice_indicator), 0, choice_indicator),
         accurate_agency_type = ifelse(agency_type == "Non District Charter Schools", "Independent Charter",
                                       ifelse(agency_type == "Private school", "Private School",
                                              ifelse(dpi_true_id %in% instrumentality, "Instrumentality",
                                                     ifelse(agency_type == "Public school" & charter_indicator == 1, "Independent Charter",
                                                            "Traditional Public School")))))


source("R/report_card_script.R")

# remove school year filter, add "last year open" field

rc1 <- rc_renamed %>%
  modify_at("school_year", factor, ordered = TRUE)  %>% 
  group_by(dpi_true_id) %>%
  summarize(last_year = max(school_year)) %>%
  ungroup() %>%
  left_join(., rc_renamed %>%
              select(locale_description, city, dpi_true_id, school_year),
            by = c("dpi_true_id", "last_year" = "school_year")) %>%
  unique() %>%
  select(-last_year)

# NOTE: locale description is "NA" in source files
# for private schools

unique_schools <- left_join(unique_schools, rc1, by = "dpi_true_id")



