#------------------ DATABASE TABLES-------------------#

# db_usr is defined in .Renviron in the Home directory
# db_pwd is defined in .Renviron in the Home directory


conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

tbl <- RPostgreSQL::dbGetQuery(
  conn = conn,
  statement = "SELECT * FROM bgt_job.jolts_comparison_2010 LIMIT 30;")

# functions
completeness_function <- function(x){
  (length(x) - sum(is.na(x)))/length(x)
}
uniqueness_function <- function(x){
  sum(!is.na(unique(x)))
}

# 2010 Profiling

# create a table
prof_2010 <- data.frame(variable = colnames(tbl),
                   completeness = numeric(length = ncol(tbl)),
                   validity = numeric(length = ncol(tbl)),
                   uniqueness = numeric(length = ncol(tbl)))

# id

# download the data
id_2010 <- RPostgreSQL::dbGetQuery(
  conn = conn,
  statement = "SELECT id FROM bgt_job.jolts_comparison_2010")

# run the tests
# completeness
prof_2010[prof_2010$variable == "id", "completeness"] <- completeness_function(id_2010$id)
# validity
prof_2010[prof_2010$variable == "id", "validity"] <- length(unique(id_2010$id))/length(id_2010$id)
# uniqueness
prof_2010[prof_2010$variable == "id", "uniqueness"] <- uniqueness_function(id_2010$id)

# jobdate

# download the data
jobdate_2010 <- RPostgreSQL::dbGetQuery(
  conn = conn,
  statement = "SELECT jobdate FROM bgt_job.jolts_comparison_2010")

# run the tests
# completeness
prof_2010[prof_2010$variable == "jobdate", "completeness"] <- completeness_function(jobdate_2010$jobdate)
# validity
library(lubridate)
prof_2010[prof_2010$variable == "jobdate", "validity"] <- (sum(jobdate_2010$jobdate %in% seq(ymd("2010-01-01"), ymd("2010-12-31"), "1 day")))/length(jobdate_2010$jobdate)
# uniqueness
prof_2010[prof_2010$variable == "jobdate", "uniqueness"] <- uniqueness_function(jobdate_2010$jobdate)

# state

# download the data
state_2010 <- RPostgreSQL::dbGetQuery(
  conn = conn,
  statement = "SELECT state FROM bgt_job.jolts_comparison_2010")

# run the tests
# completeness
prof_2010[prof_2010$variable == "state", "completeness"] <- completeness_function(state_2010$state)
# validity
state_names <- c(state.name,  "District of Columbia", "Puerto Rico", "Virgin Islands of the U.S.", 
                 "Guam", "American Samoa", "Northern Mariana Islands", "Palau","Marshall Islands",
                 "Federated States of Micronesia")

prof_2010[prof_2010$variable == "state","validity"] <- sum(state_2010$state %in% state_names)/length(state_2010$state)
# uniqueness
prof_2010[prof_2010$variable == "state", "uniqueness"] <- uniqueness_function(state_2010$state)

# soc

# download the data
soc_2010 <- RPostgreSQL::dbGetQuery(
  conn = conn,
  statement = "SELECT soc FROM bgt_job.jolts_comparison_2010")

# run the tests
# completeness
prof_2010[prof_2010$variable == "soc", "completeness"] <- completeness_function(soc_2010$soc)
# validity
prof_2010[prof_2010$variable == "soc","validity"] <- (sum(nchar(soc_2010$soc) == 7,na.rm = T) + sum(is.na(soc_2010$soc)))/length(soc_2010$soc)
# uniqueness
prof_2010[prof_2010$variable == "soc", "uniqueness"] <- uniqueness_function(soc_2010$soc)

# lat

# download the data
lat_2010 <- RPostgreSQL::dbGetQuery(
  conn = conn,
  statement = "SELECT lat FROM bgt_job.jolts_comparison_2010")

# run the tests
# completeness
prof_2010[prof_2010$variable == "lat", "completeness"] <- completeness_function(lat_2010$lat)
# validity
prof_2010[prof_2010$variable == "lat","validity"] <- (sum(lat_2010$lat > 0, na.rm = T) + sum(is.na(lat_2010$lat)))/length(lat_2010$lat)
# uniqueness
prof_2010[prof_2010$variable == "lat", "uniqueness"] <- uniqueness_function(lat_2010$lat)

# lon

# download the data
lon_2010 <- RPostgreSQL::dbGetQuery(
  conn = conn,
  statement = "SELECT lon FROM bgt_job.jolts_comparison_2010")

# run the tests
# completeness
prof_2010[prof_2010$variable == "lon", "completeness"] <- completeness_function(lon_2010$lon)
# validity
prof[prof$variable == j, "validity"]  <- (sum(tbl[, j][!is.na(tbl[,j])] < 0 |( 133 < tbl[, j][!is.na(tbl[,j])] & tbl[, j][!is.na(tbl[,j])] < 172)) + sum(is.na(tbl[,j])))/length(tbl[,j])
# uniqueness
prof_2010[prof_2010$variable == "lon", "uniqueness"] <- uniqueness_function(lon_2010$lon)

# minedu

# download the data
minedu_2010 <- RPostgreSQL::dbGetQuery(
  conn = conn,
  statement = "SELECT minedu FROM bgt_job.jolts_comparison_2010")

# run the tests
# completeness
prof_2010[prof_2010$variable == "minedu", "completeness"] <- completeness_function(minedu_2010$minedu)
# validity
prof_2010[prof_2010$variable == "minedu","validity"] <- (sum(minedu_2010$minedu %in% c(0, 12, 14, 16, 18, 21)) + sum(is.na(minedu_2010$minedu)))/length(minedu_2010$minedu)
# uniqueness
prof_2010[prof_2010$variable == "minedu", "uniqueness"] <- uniqueness_function(minedu_2010$minedu)

# maxedu

# download the data
maxedu_2010 <- RPostgreSQL::dbGetQuery(
  conn = conn,
  statement = "SELECT maxedu FROM bgt_job.jolts_comparison_2010")

# run the tests
# completeness
prof_2010[prof_2010$variable == "maxedu", "completeness"] <- completeness_function(maxedu_2010$maxedu)
# validity
prof_2010[prof_2010$variable == "maxedu","validity"] <- (sum(maxedu_2010$maxedu %in% c(0, 12, 14, 16, 18, 21)) + sum(is.na(maxedu_2010$maxedu)))/length(maxedu_2010$maxedu)
prof_2010[prof_2010$variable == "maxedu", "uniqueness"] <- uniqueness_function(maxedu_2010$maxedu)