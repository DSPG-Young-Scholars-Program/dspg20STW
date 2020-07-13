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
  
  total_region <<- total
  total_wide_region <<- spread(total, variable, value)
}

compare_years_region(2010:2019)

#########

ggplot(data=total, aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity")

# ************
ggplot(data=total, aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) + theme_minimal() + 
  scale_y_continuous(labels = scales::comma,name = "Job Estimates",seq(0, 90000000, by = 20000000)) +
  scale_x_continuous(breaks = 2010:2019) +
  labs(title = "BGT and JOLTS Job Estimates Over Time", caption = "Blue bars represent JOLTS estimates, orange bars represent BGT estimates. ", x = "", y = "Job Estimates") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#E57200","#232D4B"))


  

ggplot(subset(total, variable %in% c("bgt")), aes(x=year, y=value, group=region)) +
  geom_point(aes(color=region)) + geom_line(aes(color=region))

ggplot(subset(total, variable %in% c("jolts")), aes(x=year, y=value, group=region)) +
  geom_point(aes(color=region)) + geom_line(aes(color=region)) + ggtitle("Jolts")

# ********
ggplot(subset(total_region, variable %in% c("jolts")), aes(x=year, y=value, fill=region)) +
  geom_bar(stat="identity") + theme_minimal() + 
  scale_y_continuous(labels = scales::comma,name = "Job Estimates",seq(0, 90000000, by = 10000000)) +
  scale_x_continuous(breaks = 2010:2019) +
  labs(title = "JOLTS Job Estimates By Region Over Time", x = "") +
  scale_fill_manual(values = c("#E57200","#232D4B","#009FDF","#FDDA24")) +
  theme(legend.position="bottom")

# *******
ggplot(subset(total_region, variable %in% c("bgt")), aes(x=year, y=value, fill=region)) +
  geom_bar(stat="identity") + theme_minimal() + 
  scale_y_continuous(labels = scales::comma,name = "Job Estimates",seq(0, 90000000, by = 10000000)) +
  scale_x_continuous(breaks = 2010:2019) +
  labs(title = "BGT Job Estimates By Region Over Time", x = "") +
  scale_fill_manual(values = c("#E57200","#232D4B","#009FDF","#FDDA24")) +
  theme(legend.position="bottom")

ggplot(subset(total_region, variable %in% c("jolts")), aes(x=year, y=value, fill=region)) +
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()
