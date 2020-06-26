library(RPostgreSQL)
library(ggplot2)
library(stringr)
library(dplyr)
library(knitr)   
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
      statement = paste("SELECT COUNT(DISTINCT id) FROM bgt_job.jolts_comparison_", y, " WHERE state IN ", paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),  sep = "")
    )
    
    total[total$variable == "bgt" & total$year == y, "value"] <- tbl[, "count"]
    
    
  }
  
  print(total)
  
}  