library(RPostgreSQL)
library(ggplot2)
library(stringr)
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
validId <- function(column){
  return(sum(nchar(column) >= 7 & sum(nchar(column)) == 9) <= length(column))
}

#Validate jobdate by looking at the year (maybe go back to)
validDate <- function(column, yr){
  return(sum(str_detect(column, as.character(yr))) / length(column))
}

#Validate state
validState <- function(col){
  return(sum(col%in%state.name | col == "District of Columbia" | col == "Puerto Rico") / length(col))
}

#Validate soc
#Looks like soc is two digits followed by a dash followed by 4 digits
validSoc <- function(col){
  return(sum(str_detect(col, "\\d+-\\d\\d\\d\\d") | is.na(col)) / length(col))
}

#Validate long
#Since the US is in lat range 0 to 90 (Northern Hemisphere) I looked at the number of ranges between 0 and 90
validLat <- function(col){
  return(sum(col >= 0 & col <= 90) / length(col))
}

year <- 2010
col_names = c("id", "jobdate", "state", "soc", "socname", "lat", "lon", "minedu", "maxedu")

for(i in year){
  #create df validity, completeness, and uniqueness
  prof <<- data.frame(variable = col_names, 
                      completeness = numeric(length = length(col_names)),
                      validity = numeric(length = length(col_names)), 
                      uniqueness = numeric(length = length(col_names)))
  
  
  for(col in col_names){
    tbl <- RPostgreSQL::dbGetQuery(
      conn = conn, 
      statement = paste("SELECT ", col, " FROM bgt_job.jolts_comparison_", year, sep = ""))
    
    prof[prof$variable == col, "completeness"] <- completeness(tbl[, col])
    prof[prof$variable == col, "uniqueness"] <- getUni(tbl[, col])
    
    #testing jobdate validity
    if(col == "jobdate"){
      prof[prof$variable == col, "validity"] <- validDate(tbl[, col], year)
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
    
    
    
    
    
      
    #Still confused about how the 2011 and 2012... dataframes are made after the first iteration
    
  }
  
}




my_str <- "My date is 2010-01-10"
my_str_2 <- c("2011/10/9", "2012/05/10", "2010/05/10")

test_year <- 2010
sum(str_detect(my_str_2, as.character(test_year)))
  



























tbl <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = "SELECT * FROM bgt_job.jolts_comparison_2010 LIMIT 30;")

#2011 first 1000 columns
tbl2 <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = "SELECT * FROM bgt_job.jolts_comparison_2013 LIMIT 1000;")

#creates a dataframe with 4 columns and 10 variables to track percentages
prof_2010 <- data.frame(variable = colnames(tbl), 
                        completeness = numeric(length = ncol(tbl)),
                        validity = numeric(length = ncol(tbl)),
                        uniqueness = numeric(length = ncol(tbl)))

head(tbl$minedu)


#------------------------------------------------------------------Ignore------------------------------------------------------------------



#Completeness -------------------------------------------



my2010df <- data.frame(variables = colnames(tbl),
                       complete = numeric(length=ncol(tbl)),
                       validity = numeric(length = ncol(tbl)),
                       uniqueness = numeric(length = ncol(tbl))) 

#calculate % of completeness




my2010df$complete <- apply(tbl, MARGIN = 2, completeness)


#Uniqueness---------------------------
#Don't count NA
#Function that returns the number of unique values not including NA


my2010df$uniqueness <- apply(tbl, MARGIN = 2, getUni)


#Validity-----------------------------------------------------

#ID
#checking for length of each id in the id column; all 30 results had a length of 9
for(i in tbl$id){
  print(nchar(as.character(i)))
}

#validity for id column in my2010df is set to 100% or 1
my2010df$validity[1] = 1





#state
#Check to see if there is a value in the state column that isn't a state

for(state in tbl$state){
  if(state%in%state.name){
    print(TRUE)
  }
  
}

my2010df$validity[3] = 1 



#jobdate
#Checking to make sure that every entry is in year-month-day format



#SOC
#Assuming a valid SOC identifier is two digits, followed by a dash, followed by 4 more digits
#Looking for length 7 characters with a dash at index 3

#This function looks counts up all the SOC values that are two digits, followed by a dash, followed by 4 more digits
socValid <- function(col){
  count = 0
  for(number in col){
    if(nchar(as.character(number)) == 7 & substring(number, 3,3) == "-" | is.na(number)){
      count = count + 1
    }
  }
  
  return(count)
}

#Get percentage of valid and add it to my2010df
my2010df$validity[4] = socValid((tbl$soc)) / length(tbl$soc)
 

#socname




#lat
#For validating latitude, I checked each value to see if it was between -90 and 90 (range of latitude)

count = 0
for(value in tbl$lat){
  if(value >= -90 & value <= 90 | is.na(value)){ 
    count = count + 1
  }
}

#Assign the percentage of valid latitude values to my2010df
my2010df$validity[6] = count / length(tbl$lat)

#long
#For validating longitude, I checked each value to see if it was between -180 and 180 (range for longitude)
count2 = 0
for(value in tbl$lon){
  if(value >= -180 & value <= 180 | is.na(value)){
    count2 = count2 + 1
  }
}

my2010df$validity[7] = count2 / length(tbl$lon)


#minedu
#for minimum education I presumed that a value was valid if it was in the range 12 to 21
countMinEdu <- 0

for(value in tbl$minedu){
  if(value >= 12 & value <= 21 | is.na(value)){
    countMinEdu = countMinEdu + 1
  }
}

my2010df$validity[8] = countMinEdu / length(tbl$minedu)

#maxedu
#for maximum education I presumed that a value was valid if it was in the range 12-21
countMaxEdu <- 0

for(value in tbl$maxedu){
  if(value >= 12 & value <= 21 | is.na(value)){
    countMaxEdu = countMaxEdu + 1
  }
}

my2010df$validity[9] = countMaxEdu / length(tbl$maxedu)

#---------------------------------------------------------------------------------



x <- c("1234567", "3456", "2345678")
sum(nchar(x) == 7)
