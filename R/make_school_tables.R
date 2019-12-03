library(DBI)

con <- dbConnect(RSQLite::SQLite(), "school_db.sqlite")

tables <- dbListTables(con)

for(i in 1:length(tables)) {
  saveRDS(dbReadTable(con, tables[i]), paste("C:/Users/Spencer/OneDrive - Schools That Can Milwaukee/Data & Impact/R Files/wisconsink12/imports/",
                                             tables[i], ".rds", sep = ""))
}

schools <- readRDS("imports/schools.rds")
enrollment <- readRDS("imports/enrollment.rds")
report_cards <- readRDS("imports/report_cards.rds")
forward_exam <- readRDS("imports/forward_exam.rds")

mke_schools <- schools %>%
  filter(city == "Milwaukee" & (district_name == "Milwaukee" | accurate_agency_type != "Private School") & locale_description != "Suburb")

mke_rc <- report_cards %>%
  filter(dpi_true_id %in% mke_schools$dpi_true_id & !(report_card_type == "Private - All Students" & has_2_rc == 1))

save(list = c("schools", "enrollment", "report_cards", "forward_exam"),
     file = "C:/Users/Spencer/OneDrive - Schools That Can Milwaukee/Data & Impact/R Files/wisconsink12/data/school_data.RData")

load("data/school_data.RData")
