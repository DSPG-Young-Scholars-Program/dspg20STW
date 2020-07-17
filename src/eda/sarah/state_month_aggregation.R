
#---------------------------------------- BGT-JOLTS STATE/MONTH Aggregation-----------------------------------------------#
state <- read_xlsx("data/original/ncses_stw/jlt_statedata_q4_2019.xlsx", skip = 4)

state <- state %>% mutate(`jolts` = `Job Openings` * 1000, 
                          Hires = Hires * 1000, 
                          Quits = Quits * 1000, 
                          `Layoffs & Discharges` = `Layoffs & Discharges` * 1000, 
                          `Total Separations` = `Total Separations` * 1000, 
                          Year = as.numeric(substr(`Period (YYYYMM)`, start = 1, stop = 4)),
                          Month = as.numeric(substr(`Period (YYYYMM)`, start = 5, stop = 6)))

new_df <- state %>% 
  filter(Year > 2009 & Year < 2020) %>% 
  select(State, `jolts`, Year, Month) 

bgt <- data.frame(year = numeric(0), month = numeric(0), count= numeric(0), state = character(0))

for(year in 2010:2019){
  
  tbl <- RPostgreSQL::dbGetQuery(
    conn = conn, 
    statement = paste("SELECT EXTRACT(YEAR FROM jobdate) AS year, EXTRACT(MONTH FROM jobdate) AS month, COUNT(DISTINCT(id)), state
                      FROM bgt_job.jolts_comparison_", year, 
                      
                      " WHERE state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
                      
                      " GROUP BY state, year, month", sep = ""))
  
  bgt <- rbind(bgt, tbl)
  
}


new <- merge(new_df, bgt, by.x = c("State", "Year", "Month"), by.y = c("state", "year","month"), all = T)

colnames(new)[colnames(new) == "count"] <- "bgt"

new$diff <- new$jolts - new$bgt


# per_diff
new$per_diff <- new$diff/((new$jolts + new$bgt)/2) * 100





