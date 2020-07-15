conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))
library(readxl)
library(dplyr)
library(statebins)
library(ggplot2)


# state year
state <- read_xlsx("data/original/jlt_statedata_q4_2019.xlsx", skip = 4)

state <- state %>% mutate(`Job Openings` = `Job Openings` * 1000, 
                          Hires = Hires * 1000, 
                          Quits = Quits * 1000, 
                          `Layoffs & Discharges` = `Layoffs & Discharges` * 1000, 
                          `Total Separations` = `Total Separations` * 1000)
# virginia 2019

new_df <- state %>% filter(grepl('^2019\\d{2}$', `Period (YYYYMM)`)) %>%
  select(State, `Job Openings`) %>%
  group_by(State) %>%
  summarize(jolts = sum(`Job Openings`))

tbl <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = "SELECT COUNT(DISTINCT id), state FROM bgt_job.jolts_comparison_2019 GROUP BY state")

new <- merge(new_df, tbl, by.x = "State", by.y = "state", all.x = T)
colnames(new)[colnames(new) == "count"] <- "bgt"

new$diff <- new$jolts - new$bgt

# per_diff
new$per_diff <- new$diff/((new$jolts + new$bgt)/2) * 100

statebins(new, state_col = "State", value_col = "per_diff", palette = "Blues",
          direction =1, round=TRUE,  name = "Percent Difference") +
  theme_statebins() +
  labs(title = "Percent Difference Between JOLTS and BGT Estimates by State, 2019")




# state/month GINA START HERE
state <- read_xlsx("data/original/jlt_statedata_q4_2019.xlsx", skip = 4)

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

#######

ggplot(subset(new, State %in% c("Virginia"))) + 
  geom_point(aes(x=Year, y=bgt),color="#E57200") + 
  geom_point(aes(x=Year, y=jolts),color="#232D4B") + theme_minimal() +
  scale_y_continuous(labels = scales::comma,name = "Job Estimates",seq(0, 1000000, by = 100000)) +
  scale_x_continuous(breaks = 2010:2019) +
  labs(title = "Virginia", x = "") 
