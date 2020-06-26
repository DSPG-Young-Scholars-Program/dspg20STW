library(tidyr)
library(dplyr)
library(ggplot2)

###########--------------- Comparison of Total Jobs by Year ---------------########### 

conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))


compare_years <- function(years){
  
  total <- data.frame(
    year = years, 
    variable = c(rep("jolts", length(years)), rep ("bgt", length(years))) , 
    value = numeric(length = 2 * length(years)))
  
  jolts <- read.table("data/original/jt.data.2.JobOpenings.txt", fill = TRUE, header = TRUE)
  
  
  for(y in years){
    
    tbl <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT COUNT(DISTINCT id) FROM bgt_job.jolts_comparison_", y, " WHERE state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),  sep = ""))
    

    total[total$variable == "bgt" & total$year == y, "value"] <- tbl[, "count"]
    
    tbl2 <- jolts %>% 
      filter(series_id == "JTU00000000JOL" & year == y) %>%
      select(series_id, year, value) %>%
      mutate(value = value * 1000) %>%
      group_by(year) %>% 
      summarize(JobOpenings = sum(value))
    
    total[total$year == y & total$variable == "jolts", "value"] <- tbl2[tbl2$year == y, 'JobOpenings']
    
  }
  total_wide <<- spread(total, variable, value)
  total <<- total
}

compare_years(2010:2019)


ggplot(total_wide, aes(x= year, xend = year, y = bgt, yend = jolts)) + 
  geom_segment() + 
  geom_point(y = total_wide$jolts, color = "blue", size = 2)+
  geom_point(y = total_wide$bgt, color = "red", size = 2) +
  scale_x_continuous(breaks = 2010:2019, 
                     limits =c(2010,2019)) + 
  scale_y_continuous(breaks = seq(0, 90000000, by = 10000000), 
                     labels = c( "0", paste(seq(10, 90, by = 10), "million")), 
                     limits = c(0, 90000000),
                     expand = c(0, 0))+
  theme_classic() +
  labs(y = '', 
       x = "", 
       title = "Comparison of JOLTS and BGT Job Estimates by Year",
       subtitle= "Blue dots show JOLTS job openings estimates, \nand red dots show BGT job-ads estimates.")

###########--------------- Comparison of Total Jobs by Year and Region ---------------########### 

  
compare_years_region <- function(years){
  
  # empty data frame to fill in 
  total <- data.frame(
    region = rep(rep(c("South", "Northeast", "West", "Midwest"),10), each = 2), 
    year = rep(rep(years, each = 4), each = 2), 
    variable = rep(c("jolts", "bgt"), 40), 
    value = numeric(length = 80))
  
  # region abbr to region name look up table
  lookup <- data.frame(region = c("NE", "SO", "WE", "MW"), name = c("Northeast", "South", "West", "Midwest"))
  
  # open jolts
  jolts <- read.table("data/original/jt.data.2.JobOpenings.txt", fill = TRUE, header = TRUE)
  
  # state to region look up table
  states <- data.frame(state.name, state.region)
  states <- rbind(states, c("District of Columbia", "South"))
  levels(states$state.region)[levels(states$state.region)=="North Central"] <- "Midwest"
  
  
  for(y in years){

    # jolts
    
    region <- jolts %>% 
      filter(grepl(pattern = "JTU.+\\D{2}JOL", x = series_id) & year == y) %>%
      mutate(region = lookup$name[match(substr(series_id, start= 10, stop = 11), lookup$region)] ) %>%
      select(series_id, year, value,region)%>%
      select(year, region, value) %>%
      group_by(year, region) %>%
      summarise(value = sum(value) * 1000)
    
    total[total$year == y & total$variable == "jolts", "value"] <- region$value[match(total[total$year == y & total$variable == "jolts", "region"], region$region)]
    
    # burning glass
    
    tbl <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT COUNT(DISTINCT id), state 
                    FROM bgt_job.jolts_comparison_", y, 
                        " WHERE state 
                      IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
                        " GROUP BY state",  sep = ""))
    
    tbl <- merge(tbl, states, by.x = "state", by.y = "state.name")
    
    tbl <- tbl %>%
      select(state.region, count) %>%
      group_by(state.region) %>%
      summarise(value = sum(count)) %>%
      rename(region = state.region)
    
    total[total$year == y & total$variable == "bgt", "value"] <- tbl$value[match(total[total$year == y & total$variable == "bgt", "region"], tbl$region)]

    
  }
  
  total <<- total
  total_wide <<- spread(total, variable, value)
}

compare_years_region(2010:2019)



ggplot(total, aes(x = year, y = value, group=region, color = region)) +
  geom_line() + 
  theme_classic() + 
  scale_x_continuous(breaks = 2010:2019, 
                     limits =c(2010,2019)) + 
  scale_y_continuous(breaks = seq(0, 40000000, by = 10000000), 
                     labels = c( "0", paste(seq(10, 40, by = 10), "million")), 
                     limits = c(0, 40000000),
                     expand = c(0, 0)) +
  facet_grid(~variable)


ggplot(total_wide, aes(x= year, xend = year, y = bgt, yend = jolts, group = region)) + 
  geom_segment() + 
  geom_point(y = total_wide$jolts, color = "blue", size = 2)+
  geom_point(y = total_wide$bgt, color = "red", size = 2) +
  scale_x_continuous(breaks = 2010:2019, 
                     limits =c(2010,2019)) + 
  scale_y_continuous(breaks = seq(0, 30000000, by = 10000000), 
                     labels = c( "0", paste(seq(10, 30, by = 10), "million")), 
                     limits = c(0, 35000000),
                     expand = c(0, 0))+
  theme_classic() +
  facet_grid(~region) +
  labs(y = '', 
       x = "", 
       title = "Comparison of JOLTS and BGT Job Estimates by Year",
       subtitle= "Blue dots show JOLTS job openings estimates, and red dots show BGT job-ads estimates.")







