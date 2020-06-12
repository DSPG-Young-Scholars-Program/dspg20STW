library(RPostgreSQL)
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
#completeness <- function(x){
  #(length(x) - sum(is.na(x))) / length(x)
#}


#Uniqueness---------------------------
#Don't count NA
#Function that returns the number of unique values not including NA
getUni <- function(col){
  return(sum(!is.na(unique(col))))
}

my2010df$uniqueness <- apply(tbl, MARGIN = 2, getUni)


#Validity
 
for(i in tbl$minedu){
  print(i)
}
