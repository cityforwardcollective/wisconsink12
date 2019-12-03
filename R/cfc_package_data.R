library(DBI)

con <- dbConnect(RSQLite::SQLite(), "school_db.sqlite")

tables <- dbListTables(con)

for(i in 1:length(tables)) {
  saveRDS(dbReadTable(con, tables[i]), paste("C:/Users/Spencer/OneDrive - Schools That Can Milwaukee/Data & Impact/R Files/cityforwardcollective/data/",
                           tables[i], ".rds", sep = ""))
}