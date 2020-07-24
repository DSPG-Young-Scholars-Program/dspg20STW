
#---------------------------------- BGT-JOLTS STATE/YEAR Aggregation----------------------------------------------------#


state_year_jolts_bgt_table_maker <- function(years){
      library(readxl)
      library(dplyr)
      library(statebins)
      library(ggplot2)

      conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                                     dbname = "sdad",
                                     host = "postgis1",
                                     port = 5432,
                                     user = Sys.getenv(x = "DB_USR"),
                                     password = Sys.getenv(x = "DB_PWD"))

      state <- read_xlsx("data/ncses_stw/original/jlt_statedata_q4_2019.xlsx", skip = 4)

      state <- state %>% 
        mutate(jolts = `Job Openings` * 1000, 
                        Hires = Hires * 1000, 
                        Quits = Quits * 1000, 
                        `Layoffs & Discharges` = `Layoffs & Discharges` * 1000, 
                        `Total Separations` = `Total Separations` * 1000, 
                        year = as.numeric(substr(`Period (YYYYMM)`, start = 1, stop = 4)),
                        Month = as.numeric(substr(`Period (YYYYMM)`, start = 5, stop = 6))) %>%
        rename(state = State) %>% 
        filter(year %in% years)

      state_year <- state %>% select(state, jolts, year)%>%
        filter(year > 2009 & year < 2020)%>%
        group_by(state, year) %>%
        summarize(jolts = sum(`jolts`))

      bgt <- data.frame(year = numeric(0), bgt = numeric(0), state = character(0))
      
      for(year in years){
      
        tbl <- RPostgreSQL::dbGetQuery(
          conn = conn, 
          statement = paste("SELECT EXTRACT(YEAR FROM jobdate) AS year, COUNT(DISTINCT(id)) AS bgt, state
                    FROM bgt_job.jolts_comparison_", year, 
                            " WHERE state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
                            " GROUP BY state, year", sep = ""))
        bgt <- rbind(bgt, tbl)
      }

      new <- merge(state_year, bgt, by = c("state","year"), all.x = T)

      new$diff <- new$jolts - new$bgt

      new$per_diff <- new$diff/((new$jolts + new$bgt)/2) * 100 

      assign(paste("state_year_bgt_jolts", min(years), max(years), sep = "_"), new, .GlobalEnv)
}


#state_year_jolts_bgt_table_maker(2010:2019)

#statebins(state_year_bgt_jolts_2010_2019[state_year_bgt_jolts_2010_2019$year == 2019, ], state_col = "state", value_col = "per_diff", palette = "Blues",
#          direction =1, round=TRUE,  name = "Percent Difference") +
#  theme_statebins() +
#  labs(title = "Percent Difference Between JOLTS and BGT Estimates by State, 2019")

















                       