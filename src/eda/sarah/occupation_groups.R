library(data.table)


#summary stats table with the states ordered by percent difference 
#(smallest to largest) and across the top are the 23 major occupation groups 
# the cells will have the percent of BGT jobs ads classified into each group 

#run first
conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))
 
source("src/eda/sarah/state_year_aggregation.R")

#run second
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

#write this as a csv instead and filter by year
final_data <- merge(state_year_bgt_jolts_2010_2019[, c("state", "year", "per_diff")], bgt, by = c("state", "year"))

# will need this for the shiny app
#write.csv(final_data, "data/ncses_stw/dataForInteractiveDoc/occupation_groups.csv")


y = 2011

data <- final_data %>% 
  filter(year == y) %>%
  arrange(desc(per_diff))


colnames(final_data) <- c("State", "Year", "Percent Difference", "SOC 11", "SOC 13", "SOC 15", "SOC 17", "SOC 19", "SOC 21",
                          "SOC 23", "SOC 25", "SOC 27", "SOC 29", "SOC 31", "SOC 33", "SOC 35", "SOC 37", "SOC 39", "SOC 41",
                          "SOC 43", "SOC 45", "SOC 47", "SOC 49", "SOC 51", "SOC 53" ,"SOC 55")


