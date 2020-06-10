# 2010 Example
conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = Sys.getenv(x = "DB_USR"),
                               password = Sys.getenv(x = "DB_PWD"))

tbl <- RPostgreSQL::dbGetQuery(
  conn = conn, 
  statement = "SELECT * FROM bgt_job.jolts_comparison_2010")

prof_2010 <- data.frame(variable = colnames(tbl), 
                        completeness = numeric(length = ncol(tbl)),
                        validity = numeric(length = ncol(tbl)), 
                        uniqueness = numeric(length = ncol(tbl)))

# Completeness - completeness is a variable metric, the metric is a percentage, the number 
# of observations that have values compared to the number of observations that “should”
# have values (NA (not available) are not counted as a value).

completeness <- function(x){
    (length(x) - sum(is.na(x)))/length(x)
}

prof_2010$completeness <- apply(tbl, MARGIN = 2, completeness)

# Uniqueness - uniqueness is a variable metric, it is the number of unique valid 
# values that have been entered for a variable (NAs are not counted as unique values).

uniqueness <- function(x){
    sum(!is.na(unique(x)))
}

prof_2010$uniqueness <- apply(tbl, MARGIN = 2, uniqueness)

#Value validity - value validity is a variable metric, data elements with proper 
#values have value validity; the metric is the percentage of data elements whose 
#attributes possess values within the range expected for a legitimate entry (NAs
#are considered a valid value).



# big function

profile <- function(year){
  
  for(i in year){
    
  tbl <<- RPostgreSQL::dbGetQuery(
    conn = conn, 
    statement = paste("SELECT * FROM bgt_job.jolts_comparison_", i, " LIMIT 30", sep = ""))
  
  prof <<- data.frame(variable = colnames(tbl), 
             completeness = numeric(length = ncol(tbl)),
             validity = numeric(length = ncol(tbl)), 
             uniqueness = numeric(length = ncol(tbl)))
  
  prof$completeness <- apply(tbl, MARGIN = 2, completeness)
  prof$uniqueness <- apply(tbl, MARGIN = 2, uniqueness)
  
  assign(paste("tbl", i, sep = ""), tbl, envir = .GlobalEnv)  
  assign(paste("prof", i, sep = ""), prof, envir = .GlobalEnv)  
  
  }
}


profile(2010:2019)



