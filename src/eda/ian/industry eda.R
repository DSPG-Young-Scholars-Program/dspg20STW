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

x <- read.table("data/original/jt.industry.txt", fill = TRUE)

bgt <- RPostgreSQL::dbGetQuery(
  conn = conn,
  statement = paste("SELECT COUNT(DISTINCT(A.id)), B.sector
  FROM bgt_job.jolts_comparison_2010 A
  JOIN bgt_job.main B
  ON A.id = B.id
  WHERE A.state IN ",
  paste("(", paste(shQuote(c(state.name, "District of Columbia"), type="sh"), collapse=", "), ")", sep = ""),
" GROUP BY B.sector", sep = ""))




#difference between JTS and JTU
#Difference between JOL and JOR


