#------------------ DATABASE TABLES-------------------#

# db_usr is defined in .Renviron in the Home directory
# db_pwd is defined in .Renviron in the Home directory


conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

#---------------- Example Table -----------------------#

tbl <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = "SELECT * FROM bgt_job.jolts_comparison_2010 LIMIT 30;")
# remove LIMIT 30

# Profiling Practice 

prof_2010 <- data.frame(variable = colnames(tbl),
                        completeness = numeric(length = ncol(tbl)),
                        validity = numeric(length = ncol(tbl)),
                        uniqueness = numeric(length = ncol(tbl)))

# completeness - percentage of observations that have values
# as opposed to NAs

(length(tbl$minedu) - sum(is.na(tbl$minedu)))/length(tbl$minedu)

completeness_function <- function(x){
  (length(x) - sum(is.na(x)))/length(x)
}
# completeness_function(tbl$minedu)
# prof_2010[prof_2010$variable == "minedu","completeness"] <- completeness_function(tbl$minedu)

# prof_2010$completeness <- apply(tbl, MARGIN = 2, completeness_function)
# takes entire completeness column and runs completeness_function for each variable
# so you don't have to do it individually


# value validity - percentage of values that are within the expected
# range for a legitimite entry (NAs are valid)

# id validity (id must be unique)
prof_2010[1,3] <- length(unique(tbl$id))/length(tbl$id)

# jobdate
prof_2010[prof_2010$variable == "jobdate","validity"]<-(sum(tbl$jobdate %in% seq(ymd("2010-01-01"), ymd("2010-12-31"), "1 day")))/length(tbl$jobdate)


# state
state_names <- c(state.name,  "District of Columbia", "Puerto Rico", "Virgin Islands of the U.S.", 
  "Guam", "American Samoa", "Northern Mariana Islands", "Palau")

prof_2010[prof_2010$variable == "state","validity"] <- sum(tbl$state %in% state_names)/length(tbl$state)

# soc
prof_2010[prof_2010$variable=="soc","validity"]<-(sum(nchar(tbl$soc) == 7,na.rm=T) + sum(is.na(tbl$soc)))/length(tbl$soc)

# lat
prof_2010[prof_2010$variable=="lat","validity"]<-(sum(tbl$lat > 0, na.rm=T) + sum(is.na(tbl$lat)))/length(tbl$lat)

# lon
prof_2010[prof_2010$variable=="lon","validity"]<-(sum(tbl$lon < 0, na.rm=T) + sum(is.na(tbl$lon)))/length(tbl$lon)

# minedu
prof_2010[prof_2010$variable=="minedu","validity"]<-(sum(tbl$minedu %in% c(12, 14, 16, 18, 21)) + sum(is.na(tbl$minedu)))/length(tbl$minedu)

# maxedu
prof_2010[prof_2010$variable=="maxedu","validity"]<-(sum(tbl$maxedu %in% c(12, 14, 16, 18, 21)) + sum(is.na(tbl$maxedu)))/length(tbl$maxedu)

# uniqueness - number of unique valid values entered for a variable
# NA is not a unique value

# sum of how many values are unique/not NA
uniqueness_function <- function(x){sum(!is.na(unique(x)))}
# prof_2010$uniqueness <- apply(tbl, MARGIN = 2, uniqueness_function)