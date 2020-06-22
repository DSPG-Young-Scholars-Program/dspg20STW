library(tidyr)
library(dplyr)

conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

# state level data frame

state= "Virginia"
state_tbl <- function(year, state){
  
  final <- data.frame(year = character(), month = character(), jobads = numeric(0))
  
  for(i in year){
  
    tbl <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT id, jobdate FROM bgt_job.jolts_comparison_", i, " WHERE state = '", state, "'", sep = ""))

    
    tbl$id <- as.factor(tbl$id)

    tbl <- tbl %>% 
      separate(jobdate, into = c("year", "month"), sep = "-", extra = "drop", remove = FALSE)  %>% 
      group_by(year, month) %>% 
      summarize(jobads = length(id))
  
    final <- rbind(final, tbl)
    
    }
    
  assign(state, final, envir = .GlobalEnv)
  
}


state_tbl(year = 2010:2019, state = "Virginia")


library(ggplot2)

Virginia <- Virginia %>% unite("time", c(year, month),sep = "") %>% mutate(time = as.numeric(time))

ggplot(Virginia, aes(x= time, y = jobads )) +
  geom_line()
  


