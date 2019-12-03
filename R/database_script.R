library(DBI)

rm(list = ls())
school_db <- dbConnect(RSQLite::SQLite(), "school_db.sqlite")

source("./R/unique_schools.R")

if(!dbExistsTable(school_db, "schools")) {
  dbSendQuery(school_db, 
              "CREATE TABLE schools (
    locale_description CHAR,
    city  CHAR,
    dpi_true_id  CHAR PRIMARY KEY,
    school_name  CHAR,
    agency_type  CHAR,
    district_name  CHAR,
    county  CHAR,
    choice_indicator  BOOLEAN,
    charter_indicator BOOLEAN,
    accurate_agency_type  CHAR,
    last_year_open CHAR
  )")
  dbAppendTable(school_db, "schools", unique_schools)
  
} else {
  ids <- dbReadTable(school_db, "schools") %>%
    select(dpi_true_id)
  
  add_schools <- unique_schools %>%
    filter(!dpi_true_id %in% ids$dpi_true_id)
  
  dbAppendTable(school_db, "schools", add_schools)
  
}


dbWriteTable(school_db, "enrollment", only_enrollment, overwrite = TRUE)

dbWriteTable(school_db, "report_cards", rc_renamed, overwrite = TRUE)

source("R/full_forward.R")

dbWriteTable(school_db, "forward_exam", forward_exam, overwrite = TRUE)

dbDisconnect(school_db)

rm(list = ls())

