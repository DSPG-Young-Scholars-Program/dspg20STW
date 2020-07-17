conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))


y = 2019

tbl <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = paste("SELECT COUNT(DISTINCT id) as bgt, 
                      EXTRACT(YEAR FROM jobdate) AS year,
                      state, 
                      CASE WHEN minedu IS NULL THEN 'missingBach'
                            WHEN minedu < 16 THEN 'noBachRequired'
                            WHEN minedu >= 16 THEN 'bachRequired'
                           END AS bach
                    FROM bgt_job.jolts_comparison_", y, 
                    " WHERE state 
                      IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
                    " GROUP BY state, year, bach",  sep = ""))
library(dplyr)
library(tidyr)

tbl %>% spread(key = bach, value = bgt) %>% mutate(tbl[, c()])

