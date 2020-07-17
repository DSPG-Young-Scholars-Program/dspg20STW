conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

source("src/eda/sarah/state_year_aggregation.R")

state_year_jolts_bgt_table_maker(2010:2019)

bgt <- data.frame()
for(year in 2010:2019){
  library(dplyr)
  library(tidyr)
 
  tbl <- RPostgreSQL::dbGetQuery(
    conn = conn, 
    statement = paste("SELECT EXTRACT(YEAR FROM jobdate) AS year, state, SUBSTRING(soc from 1 for 2) AS occ, COUNT(DISTINCT(id)) AS bgt
                    FROM bgt_job.jolts_comparison_", year, 
                      " WHERE state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
                      " GROUP BY  year, occ, state", sep = ""))
  
  tbl <- tbl %>% spread(key = occ, value = bgt)
  tbl <- tbl %>% mutate(round(tbl[, -c(1:2)]/rowSums(x = tbl[, -c(1:2)]), 4))
  
  bgt <- rbind(bgt, tbl)
}


final_data <- merge(state_year_bgt_jolts_2010_2019[, c("state", "year", "per_diff")], bgt, by = c("state", "year"))


y = 2010

data <- final_data %>% 
  filter(year == y) %>%
  arrange(desc(per_diff))






