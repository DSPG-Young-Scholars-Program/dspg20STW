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

state <- read_xlsx("data/original/jlt_statedata_q4_2019.xlsx", skip = 4)

state <- state %>% mutate(`Job Openings` = `Job Openings` * 1000, 
                          Hires = Hires * 1000, 
                          Quits = Quits * 1000, 
                          `Layoffs & Discharges` = `Layoffs & Discharges` * 1000, 
                          `Total Separations` = `Total Separations` * 1000)
# virginia 2018

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


                       