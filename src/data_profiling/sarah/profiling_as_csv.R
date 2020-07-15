library(lubridate)

conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))
completeness <- function(x){
  round(sum(!is.na(x))/length(x), 4)
}

uniqueness <- function(x){
  sum(!is.na(unique(x)))
}


profile <-function(year){
  
  cols <<- c("id","jobdate", "state",   "soc", "socname", "lat", "lon",  "minedu",  "maxedu" )
  
  prof <<- data.frame(variable = cols, 
                      completeness = numeric(length = length(cols)),
                      validity = numeric(length = length(cols)), 
                      uniqueness = numeric(length = length(cols)))
  for (j in cols){
    
    tbl <<- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT " , j, " FROM bgt_job.jolts_comparison_", year, sep = ""))
    
    prof[prof$variable == j, "completeness"] <- completeness(tbl[, j])
    
    prof[prof$variable == j, "uniqueness"] <- uniqueness(tbl[, j])
    
    # value validity
    
    if(j == "id"){
      
      prof[prof$variable == j, "validity"] <- length(unique(tbl[, j]))/length(tbl[, j])
      
    } else if (j == "jobdate"){
      
      prof[prof$variable == j, "validity"] <- sum(tbl[, j] %in% seq(ymd(paste(as.character(year), "-01-01", sep = "")), 
                                                                    ymd(paste(as.character(year), "-12-31", sep = "")), '1 day'))/length(tbl[, j])
      
    } else if (j == "state"){
      
      prof[prof$variable == j, "validity"] <- (sum(tbl[, j] %in% c(state.name, "District of Columbia", "Puerto Rico", "Virgin Islands of the U.S.", "Guam", "American Samoa", "Northern Mariana Islands", "Palau", "Marshall Islands", "Federated States of Micronesia")) + sum(is.na(tbl[, j])))/length(tbl[, j])
      
    } else if (j == "soc"){
      
      prof[prof$variable == j, "validity"] <- (sum(grepl("\\d{2}\\-\\d{4}", tbl[, j][!is.na(tbl[, j])])) + sum(is.na(tbl[, j])))/length(tbl[, j])
      
    } else if (j == "socname"){
      
      prof[prof$variable == j, "validity"]  <- (sum(!grepl(pattern = "\\d", tbl[, j][!is.na(tbl[, j])])) + sum(is.na(tbl[, j])))/length(tbl[, j])
      
    } else if (j == "lat"){
      
      prof[prof$variable == j, "validity"]  <- (sum(tbl[,j][!is.na(tbl[, j])] > 0) + sum(is.na(tbl[, j])))/length(tbl[, j])
      
    } else if (j == "lon"){
      
      prof[prof$variable == j, "validity"]  <- (sum(tbl[, j][!is.na(tbl[,j])] < 0 |( 133 < tbl[, j][!is.na(tbl[,j])] & tbl[, j][!is.na(tbl[,j])] < 172)) + sum(is.na(tbl[,j])))/length(tbl[,j])
      
    } else if (j == "minedu"|j == "maxedu"){
      
      prof[prof$variable == j, "validity"]  <- (sum(tbl[, j][!is.na(tbl[, j])] %in% c(0, 12, 14, 16, 18, 21)) + sum(is.na(tbl[, j])))/length(tbl[, j])
    }
    
    
  }
  
  assign(paste("prof", year, sep = ""), prof, envir = .GlobalEnv)  
}

library(data.table)

profile(2010)
fwrite(prof2010, "data/ncses_stw/prof2010.csv", row.names = F)
profile(2011)
write.csv(prof2011, "data/profiling_tables/prof2011.csv", row.names = F)
profile(2012)
write.csv(prof2012, "data/profiling_tables/prof2012.csv", row.names = F)
profile(2013)
write.csv(prof2013, "data/profiling_tables/prof2013.csv", row.names = F)
profile(2014)
profile(2015)
profile(2016)
profile(2017)
profile(2018)
proflie(2019)






