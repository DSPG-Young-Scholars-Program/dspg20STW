#------------------ DATABASE TABLES-------------------#

# db_usr is defined in .Renviron in the Home directory
# db_pwd is defined in .Renviron in the Home directory


conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

# functions
completeness_function <- function(x){
  (length(x) - sum(is.na(x)))/length(x)
}
uniqueness_function <- function(x){
  sum(!is.na(unique(x)))
}

library(lubridate)

prof_function <- function(year){
  # create a table
  cols <<- c("id","jobdate", "state",   "soc", "socname", "lat", "lon",  "minedu",  "maxedu" )
  prof <<- data.frame(variable = (cols),
                          completeness = numeric(length = length(cols)),
                          validity = numeric(length = length(cols)),
                          uniqueness = numeric(length = length(cols)))
  for (c in cols){
    tbl <<- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT " , c, " FROM bgt_job.jolts_comparison_", year, sep = ""))
    
    prof[prof$variable == c, "completeness"] <- completeness_function(tbl[,c])
    prof[prof$variable == c, "uniqueness"] <- uniqueness_function(tbl[,c])
    
    if(c == "id"){
      prof[prof$variable == c, "validity"] <- length(unique(tbl[,c]))/length(tbl[,c])
    }
    
    else if(c == "jobdate"){
      prof[prof$variable == c, "validity"] <- sum(tbl[, c] %in% seq(ymd(paste(as.character(year), "-01-01", sep = "")), ymd(paste(as.character(year), "-12-31", sep = "")), '1 day'))/length(tbl[, c])
    }
    
    else if(c == "state"){
      state_names <- c(state.name,  "District of Columbia", "Puerto Rico", "Virgin Islands of the U.S.", 
                       "Guam", "American Samoa", "Northern Mariana Islands", "Palau","Marshall Islands",
                       "Federated States of Micronesia")
      
      prof[prof$variable == c,"validity"] <- sum(tbl[,c] %in% state_names)/length(tbl[,c])
    }
    
    else if(c == "soc"){
      prof[prof$variable == c,"validity"] <- (sum(nchar(tbl[,c]) == 7,na.rm = T) + sum(is.na(tbl[,c])))/length(tbl[,c])
    }
    
    else if(c == "lat"){
      prof[prof$variable == c,"validity"] <- (sum(tbl[,c] > 0, na.rm = T) + sum(is.na(tbl[,c])))/length(tbl[,c])
    }
    
    else if(c == "lon"){
      prof[prof$variable==c,"validity"]<-(sum(tbl[,c] < 0, na.rm=T) + sum(is.na(tbl[,c])))/length(tbl[,c])
    }
    
    else if(c == "minedu"){
      prof[prof$variable == c,"validity"] <- (sum(tbl[,c] %in% c(0, 12, 14, 16, 18, 21)) + sum(is.na(tbl[,c])))/length(tbl[,c])
    }
    
    else if(c == "maxedu"){
      prof[prof$variable == c,"validity"] <- (sum(tbl[,c] %in% c(0, 12, 14, 16, 18, 21)) + sum(is.na(tbl[,c])))/length(tbl[,c])
    }
  }
  assign(paste("prof", year, sep = ""), prof, envir = .GlobalEnv)
}


prof_function(2010)
prof_function(2011)
