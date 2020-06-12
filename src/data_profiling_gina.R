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

# Profiling

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

prof_2010$completeness <- apply(tbl, MARGIN = 2, completeness_function)
# takes entire completeness column and runs completeness_function for each variable
# so you don't have to do it individually


# value validity - percentage of values that are within the expected
# range for a legitimite entry (NAs are valid)

# uniqueness - number of unique valid values entered for a variable
# NA is not a unique value

# sum of how many values are unique/not NA
uniqueness_function <- function(x){sum(!is.na(unique(x)))}
prof_2010$uniqueness <- apply(tbl, MARGIN = 2, uniqueness_function)