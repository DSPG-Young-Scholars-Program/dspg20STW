library(dplyr)
library(tidyr)

conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

bach <- data.frame()
for(year in 2010:2019){
  
    tbl <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT COUNT(DISTINCT id) as bgt, 
                          EXTRACT(YEAR FROM jobdate) AS year,
                          state, 
                          CASE WHEN minedu IS NULL THEN 'missingBach'
                                WHEN minedu < 16 THEN 'noBachRequired'
                                WHEN minedu >= 16 THEN 'bachRequired'
                               END AS bach
                        FROM bgt_job.jolts_comparison_", year, 
                        " WHERE state 
                          IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
                        " GROUP BY state, year, bach",  sep = ""))
    
    tbl <- tbl %>% spread(key = bach, value = bgt)
    tbl <- tbl[,c(1,2,5,3,4)]
    tbl <- tbl %>% mutate(round(tbl[, -c(1:2)]/rowSums(x = tbl[, -c(1:2)]), 4))
    
    bach <- rbind(tbl, bach)

}


occ <- data.frame()
for(year in 2010:2019){
  
    tbl <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT EXTRACT(YEAR FROM jobdate) AS year, state, SUBSTRING(soc from 1 for 2) AS occ, COUNT(DISTINCT(id)) AS bgt
                      FROM bgt_job.jolts_comparison_", year, 
                        " WHERE state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
                        " GROUP BY  year, occ, state", sep = ""))
    
    tbl <- tbl %>% spread(key = occ, value = bgt)
    tbl <- tbl %>% mutate(round(tbl[, -c(1:2)]/rowSums(x = tbl[, -c(1:2)]), 4))
    
    occ <- rbind(occ, tbl)
}

final_data <- merge(bach[, c("state", "year", "noBachRequired")], occ,  by = c("state", "year"))

y = 2019

data <- final_data %>% 
  filter(year == y) %>%
  arrange(desc(noBachRequired))





