library(RPostgreSQL)
library(ggplot2)
library(stringr)
library(dplyr)
library(knitr) 
library(tidyr)
library(gridExtra)
#------------------ DATABASE TABLES-------------------#

# db_usr is defined in .Renviron in the Home directory
# db_pwd is defined in .Renviron in the Home directory

#Connects to the database
conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(), 
                               dbname = "sdad",
                               host = "postgis1", 
                               port = 5432, 
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

tbl2010 <- RPostgreSQL::dbGetQuery(
  conn = conn,
  statement = "SELECT * FROM bgt_job.jolts_comparison_2010;")

tbl2011 <- RPostgreSQL::dbGetQuery(
  conn = conn,
  statement = "SELECT * FROM bgt_job.jolts_comparison_2011;")


#Function that compares the years between BGT and Jolts
compare_years <- function(years){
  
  #Creates a dataframe with three columns: year(2010, 2011, etc), variable (Jolts or BGT), and value
  #total of 20 rows, 10 for JOLTS and 10 for BGT
  total <- data.frame(
    year = years,
    variable = c(rep("jolts", length(years)), rep("bgt", length(years))),
    value = numeric(length = 2 * length(years))
  )
  
  
  #Reading in jolts job openings txt
  jolts <- read.table("data/original/jt.data.2.JobOpenings.txt", fill = TRUE, header = TRUE)
  
  #for loop that iterates through all the years
  for(y in years){
    
    #Query that looks at the BGT data, and grabs the data for the year where for any of the states or DC (since JOLTS only deals with those locations)
    tbl <- RPostgreSQL::dbGetQuery( 
      conn = conn,
      #This line of code gets the count of distinct job ids from the bgt data based on year and location within the US
      statement = paste("SELECT COUNT(DISTINCT id) FROM bgt_job.jolts_comparison_", y, " WHERE state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),  sep = "")
    )
    
    total[total$variable == "bgt" & total$year == y, "value"] <- tbl[, "count"]
    
    
    tbl2 <- jolts%>%
      #Querying to find rows with a specific id and y
      filter(series_id == "JTS00000000JOL" & year == y) %>%
      #After getting the specified rows, select only the series_id, year, and value column
      select(series_id, year, value) %>%
      #changes or mutates the value column by multiplying each value by 1000 (since the numbers are in 1000s in the jolts data)
      mutate(value = value * 1000) %>%
      group_by(year) %>%
      summarize(JobOpenings = sum(value))
    
    
    total[total$variable == "jolts" & total$year == y, "value"] <- tbl2[tbl2$year == y, 'JobOpenings']
    
    
  }
  total_wide <<- spread(total, variable, value)
  
  total <<- total 
  
   
  
}

compare_years(2010:2019)


#New graph for presentation
ggplot(total, aes(x = year, y = value)) + 
  geom_point(colour = "#232D4B") + 
  facet_wrap(~ variable) + 
  ggtitle("Total Job Openings BGT vs Jolts") + 
  labs(y = "Number of Job Openings") +  
  scale_x_continuous(breaks = c(2010,2012,2014,2016,2018)) +
  scale_y_continuous(breaks = seq(0,90000000, 10000000)) +
  theme(plot.title = element_text(hjust = .5)) + 
  theme(strip.background = element_rect(fill="#E57200"))


#Looking at the quantiles
joltData <- total[total$variable == 'jolts', ]
print(quantile(joltData$value))

bgt_data <- total[total$variable == 'bgt', ]
print(quantile(bgt_data$value))


#comparing over region
compare_years_region <- function(years){
  
  total <- data.frame(
    region = rep(rep(c("South", "Northeast", "West", "Midwest"), 10), each = 2),
    year = rep(rep(years, each = 4), each = 2), 
    variable = rep(c("jolts", "bgt"), 40), 
    value = numeric(length = 80)
  )
  
  
  
  lookup <- data.frame(region = c("NE", "SO", "WE", "MW"), name = c("Northeast", "South", "West", "Midwest"))
  
  
  
  jolts <- read.table("data/original/jt.data.2.JobOpenings.txt", fill = TRUE, header = TRUE)
  
  states <- data.frame(state.name, state.region)
  states <- rbind(states, c("District of Columbia", "South"))
  levels(states$state.region)[levels(states$state.region) == "North Central"] <- "Midwest"
  
  for(y in years){
    
    #jolts
    
    region <- jolts %>%
      filter(grepl(pattern = "JTU.+\\D{2}JOL", x = series_id) & year == y) %>%
      mutate(region = lookup$name[match(substr(series_id, start = 10, stop = 11), lookup$region)]) %>%
      select(year, region, value) %>%
      group_by(year, region) %>%
      summarise(value = sum(value) * 1000)
    
    total[total$year == y & total$variable == "jolts", "value"] <- region$value[match(total[total$year == y & total$variable == "jolts", "region"], region$region)]
    
    
    #bgt
    
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

#--------------------------------------------------------





#boxplot visual where each boxplot represents a region and is color coordinated based on bgt or jolts; y axis is value
options(scipen = 10000)
ggplot(total, aes(x = factor(region), y = value, fill = variable)) + 
  geom_boxplot() + ggtitle("BGT vs Jolts Job Openings per Region") + 
  theme(plot.title = element_text(hjust = .5)) + 
  labs(x = "Region", y = "Job Openings between 2010-2019") +
  scale_y_continuous(breaks = seq(0,30000000, 3000000)) +
  theme_classic() 
#For midwest jolts estimates, 50% of total job openings between 2010 and 2019 reported 13,759,000 or less openings


ggplot(total, aes(x = year, y = value, color = region)) + 
  geom_point() + 
  facet_grid(rows = vars(variable)) + 
  ggtitle("Total Job Openings BGT vs Jolts per Region") + 
  labs(y = "Number of Job Openings") + 
  scale_y_continuous(breaks = seq(0, 30000000, 3000000)) +  
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) +
  theme_classic()










#Industry------------- 
x <- read.table("data/original/jt.industry.txt", fill = TRUE)

bgt <- RPostgreSQL::dbGetQuery(
  conn = conn,
  statement = "SELECT A.id,  B.sector
  FROM bgt_job.jolts_comparison_2010 A
  JOIN bgt_job.main B
  ON A.id = B.id")
 