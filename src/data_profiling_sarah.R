# 2010 Example

library(lubridate)
conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

tbl <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = "SELECT id FROM bgt_job.jolts_comparison_2010 LIMIT 30")


# Completeness - completeness is a variable metric, the metric is a percentage, the number 
# of observations that have values compared to the number of observations that “should”
# have values (NA (not available) are not counted as a value).

completeness <- function(x){
    sum(!is.na(x))/length(x)
}


# Uniqueness - uniqueness is a variable metric, it is the number of unique valid 
# values that have been entered for a variable (NAs are not counted as unique values).

uniqueness <- function(x){
    sum(!is.na(unique(x)))
}


#Value validity - value validity is a variable metric, data elements with proper 
#values have value validity; the metric is the percentage of data elements whose 
#attributes possess values within the range expected for a legitimate entry (NAs
#are considered a valid value).

cols <- c("id","jobdate", "state",   "soc", "socname", "lat", "lon",  "minedu",  "maxedu" )

year = 2011

for(i in year){
    
   prof <<- data.frame(variable = cols, 
                        completeness = numeric(length = length(cols)),
                        validity = numeric(length = length(cols)), 
                        uniqueness = numeric(length = length(cols)))
   for (j in cols){

      tbl <<- RPostgreSQL::dbGetQuery(
        conn = conn, 
        statement = paste("SELECT " , j, " FROM bgt_job.jolts_comparison_", i, sep = ""))
      
      prof[prof$variable == j, "completeness"] <- completeness(tbl[, j])
      
      prof[prof$variable == j, "uniqueness"] <- uniqueness(tbl[, j])
      
      # value validity
      
      if(j == "id"){
        
        prof[prof$variable == j, "validity"] <- length(unique(tbl[, j]))/length(tbl[, j])
      
      } else if (j == "jobdate"){
        
        prof[prof$variable == j, "validity"] <- sum(tbl[, j] %in% seq(ymd(paste(as.character(year), "-01-01", sep = "")), 
                             ymd(paste(as.character(year), "-12-31", sep = "")), '1 day'))/length(tbl[, j])
        
      } else if (j == "state"){
        
        prof[prof$variable == j, "validity"] <- sum(tbl[, j] %in% c(state.name, "District of Columbia", "Puerto Rico", "Virgin Islands of the U.S.", "Guam", "American Samoa", "Northern Mariana Islands", "Palau"))/length(tbl[, j])
      
      } else if (j == "soc"){
        
        prof[prof$variable == j, "validity"] <- (sum(nchar(tbl[, j]) == 7, na.rm = TRUE) + sum(is.na(tbl[, j])))/length(tbl[, j])
      
      } else if (j == "socname"){
        
        prof[prof$variable == j, "validity"]  <- (sum(grepl(pattern = "(.*[A-za-z].*)", tbl[, j][!is.na(tbl[, j])])) + sum(is.na(tbl[, j])))/length(tbl[, j])
      
      } else if (j == "lat"){
        
        prof[prof$variable == j, "validity"]  <- (sum(!is.na(tbl[, j]) > 0) + sum(is.na(tbl[, j])))/length(tbl[, j])
      
      } else if (j == "lon"){
        
        prof[prof$variable == j, "validity"]  <- (sum(!is.na(tbl[,j] < 0) ) + sum(is.na(tbl[,j])))/length(tbl[,j])
        
      } else if (j == "minedu"|j == "maxedu"){
        
        prof[prof$variable == j, "validity"]  <- (sum(tbl[, j][!is.na(tbl[, j])] %in% c(12, 14, 16, 18, 21)) + sum(is.na(tbl[, j])))/length(tbl[, j])
      }
      

   }
   
  assign(paste("prof", i, sep = ""), prof)  
}


