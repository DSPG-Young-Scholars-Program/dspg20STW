library(RPostgreSQL)
library(ggplot2)
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

#---------------- Example Table -----------------------#

tbl <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = "SELECT * FROM bgt_job.jolts_comparison_2010 LIMIT 30;")

#creates a dataframe with 4 columns and 10 variables to track percentages
prof_2010 <- data.frame(variable = colnames(tbl), 
                        completeness = numeric(length = ncol(tbl)),
                        validity = numeric(length = ncol(tbl)),
                        uniqueness = numeric(length = ncol(tbl)))

head(tbl$minedu)


#------------------------------------------------------------------Code Below------------------------------------------------------------------



#Completeness -------------------------------------------

#input column of values from dataframe
calcCompleteness <- function(col){
  #initialized to be 0
  numberOfNA <- 0
  #for loop that runs through each value in the column
  for(i in col){
    #if value, represented by i, is NA...
    if(is.na(i)){
      #increment count by 1
      numberOfNA = numberOfNA + 1
    }
  }
  
  #return the % of complete values in the column
  return((length(col) - numberOfNA) / length(col))
  
  
}

my2010df <- data.frame(variables = colnames(tbl),
                       complete = numeric(length=ncol(tbl)),
                       validity = numeric(length = ncol(tbl)),
                       uniqueness = numeric(length = ncol(tbl))) 

#calculate % of completeness


#sarah's %completeness function
completeness <- function(x){
  (length(x) - sum(is.na(x))) / length(x)
}

my2010df$complete <- apply(tbl, MARGIN = 2, completeness)


#Uniqueness---------------------------
#Don't count NA
#Function that returns the number of unique values not including NA
getUni <- function(col){
  return(sum(!is.na(unique(col))))
}

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

#created a horizontal barchart that shows the count for each occupation (including NA)
ggplot(tbl, aes(x = socname)) + geom_bar() + coord_flip()

#I manually counted each category to see if there was a value that wasn't an occupation; in the first 30 rows of the 2010 dataset, all were valid
my2010df$validity[5] = 1


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
