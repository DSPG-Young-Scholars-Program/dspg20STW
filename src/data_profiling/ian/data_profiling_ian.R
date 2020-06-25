library(RPostgreSQL)
library(ggplot2)
library(stringr)
library(dplyr)
library(knitr)   
#------------------ DATABASE TABLES-------------------#

# db_usr is defined in .Renviron in the Home directory
# db_pwd is defined in .Renviron in the Home directory
 
#testing

#Connects to the database
conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(), 
                               dbname = "sdad",
                               host = "postgis1", 
                               port = 5432, 
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))  
 
#--------------------------------Code------------------------------------

#functions

#sarah's %completeness function
completeness <- function(x){
  (length(x) - sum(is.na(x))) / length(x) 
}

#My uniqueness function
getUni <- function(col){
  return(sum(!is.na(unique(col)))) 
}

#id
#Assuming that a valid jobid is between 6 12 characters long
validId <- function(col){
  return(sum(between(nchar(col),6,12) | is.na(col)) / length(col))  
}

#Validate jobdate by looking at the year (maybe go back to)
validDate <- function(column, yr){
  return(sum(str_detect(column, as.character(yr)) | is.na(column)) / length(column)) 
}

#Validate state
#c(state.name, "District of Columbia", "Puerto Rico", "Virgin Islands of the U.S.", "Guam", "American Samoa", "Northern Mariana Islands", "Palau")


other_places = c("District of Columbia", "Puerto Rico", "Virgin Islands of the U.S.", "Guam", "American Samoa", "Northern Mariana Islands", "Palau", "Marshall Islands", "Federated States of Micronesia")
validState <- function(col){
  return(sum(col%in%state.name | col%in%other_places | is.na(col)) / length(col))  
}

#Validate soc
#Looks like soc is two digits followed by a dash followed by 4 digits
validSoc <- function(col){
  return(sum(str_detect(col, "\\d+-\\d\\d\\d\\d") | is.na(col)) / length(col))
}


#validate socname
#For this variable I am just going to see if any occupation names have any digits in them. If they do, they won't be counted as valid
validSocname <- function(col){
  return(sum(!str_detect(col, "[:digit:]") | is.na(col)) / length(col))
}


#Validate lat
#Since the US is in lat range 0 to 90 (Northern Hemisphere) I looked at the number of ranges between 0 and 90
validLat <- function(col){
  return(sum(col >= 0 & col <= 90 | is.na(col)) / length(col))
}

#Validate lon
#Since the US is West of the prime meridian, valid longitude values will be in the negatives (or between 133 and 172)
validLong <- function(col){
  return(sum(sign(col) == -1 | is.na(col) | between(col, 132, 173)) / length(col))
}

#validate minedu
#Does 0 mean there are no education requirements? #add zero
#12,14,16,18,and 21 were used to represent edu for all of the years. 2018 and 2019 had 0 as an entry, however, so I did not count these as valid
valid <- c(0,12, 14, 16, 18, 21)
validMinEdu <- function(col){
  return(sum(col%in%valid | is.na(col)) / length(col))
} 

#validate maxedu 
#12,14,16,18,and 21 were used to represent edu for all of the years. 2018 and 2019 had 0 as an entry, however, so I did not count these as valid
valid <- c(0,12, 14, 16, 18, 21)
validMaxEdu <- function(col){
  return(sum(col%in%valid | is.na(col)) / length(col))  
} 


year <- 2010:2019 
col_names = c("id","jobdate", "state", "soc", "socname", "lat", "lon", "minedu", "maxedu")

for(i in year){
  #create df validity, completeness, and uniqueness
  prof <<- data.frame(variable = col_names, 
                      completeness = numeric(length = length(col_names)),
                      validity = numeric(length = length(col_names)), 
                      uniqueness = numeric(length = length(col_names)))
  
  
  for(col in col_names){
    tbl <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT ", col, " FROM bgt_job.jolts_comparison_", i, sep = ""))
    
    prof[prof$variable == col, "completeness"] <- completeness(tbl[, col])
    prof[prof$variable == col, "uniqueness"] <- getUni(tbl[, col])
    
    #testing jobdate validity
    if(col == "jobdate"){
      prof[prof$variable == col, "validity"] <- validDate(tbl[, col], i)
    }
    
    #Testing for state
    if(col == "id"){
      prof[prof$variable == col, "validity"] <- validId(tbl[, col])
    }
    
    if(col == "state"){
      prof[prof$variable == col, "validity"] <- validState(tbl[, col])
    }
    
    if(col == "soc"){
      prof[prof$variable == col, "validity"] <- validSoc(tbl[, col]) 
    }
    
    if(col == "socname"){
      prof[prof$variable == col, "validity"] <- validSocname(tbl[, col]) 
    }
    
    if(col == "lat"){
      prof[prof$variable == col, "validity"] <- validLat(tbl[, col])
    }
    
    if(col == "lon"){
      prof[prof$variable == col, "validity"] <- validLong(tbl[, col])
    }
    
    if(col == "minedu"){
      prof[prof$variable == col, "validity"] <- validMinEdu(tbl[, col])
    }
    
    if(col == "maxedu"){
      prof[prof$variable == col, "validity"] <- validMaxEdu(tbl[, col])
    }
  } 
  assign(paste("prof", i, sep = ""), prof)
  
  
}
 
  
knitr::kable(prof2010) 
knitr::kable(prof2011)
knitr::kable(prof2012)
knitr::kable(prof2013)
knitr::kable(prof2014)
knitr::kable(prof2015)
knitr::kable(prof2016) 
knitr::kable(prof2017)
knitr::kable(prof2018)
knitr::kable(prof2019)
 
































